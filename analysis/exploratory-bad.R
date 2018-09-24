##
## exploratory.R, load + exploratory analysis:
##
##     - twitter
##     - nasdaq (ndx)
##

## set project cwd: only execute in RStudio
if (nzchar(Sys.getenv('RSTUDIO_USER_IDENTITY'))) {
  cwd <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  setwd(cwd)
}

## create ignored directories
dir.create(file.path(cwd, 'visualization'), showWarnings = FALSE)

## utility functions
devtools::install_local(paste(cwd, sep='', '/packages/customUtility'))
library('customUtility')

##
## load packages
##
## tidytext, afinn list of English words rated for valence with an integer,
##     between minus five (negative) and plus five (positive)
##
## tidyverse, allows 'group_by'
## RcppParallel, is required by 'text2vec'
##
load_package(c(
  'data.table',
  'tidytext',
  'tidyverse',
  'gtools',
  'RcppParallel',
  'text2vec',
  'jsonlite',
  'readtext',
  'rlist',
  'naivebayes'
))

## local variables
articles = list()
dtm.final = list()

## create dataframes
df.wikipedia = load_data(paste0(cwd, '/data/wikipedia'), remove=TRUE, type='json')
df.twitter = load_data(paste0(cwd, '/data/twitter'), remove=TRUE, type='json')
df.ndx = load_data(paste0(cwd, '/data/nasdaq/^ndx.csv'), remove=TRUE, type='csv')

## create vocabulary
df.wikipedia.sample = fromJSON('2016-08-01--sample-train.json')
df.wikipedia.sample = as.matrix(df.wikipedia.sample[[1]])[[3]]

## generate tfidf dtm per article
for (filename in df.wikipedia.sample$article) {
  print(paste0(cwd, '/data/wikipedia/articles/', filename, '.txt'))

  ## read article
  tryCatch({
    fp = readtext(paste0(cwd, '/data/wikipedia/articles/', filename, '.txt'))
  }, warning = function(warning_condition) {
    print(paste0('warning: ', warning_condition))
  }, error = function(error_condition) {
    print(paste0('error: ', error_condition))
  })

  if (!is.null(fp) && fp[[1]] != '' && fp[[2]] != '') {
    ## create vocabulary
    it_train = itoken(
      fp[[2]],
      preprocessor = tolower,
      tokenizer = word_tokenizer
    )
    vocab = create_vocabulary(it_train)
    
    ## document term matrix
    vectorizer = vocab_vectorizer(vocab)
    dtm = create_dtm(it_train, vectorizer)

    f = tools::file_path_sans_ext(fp[[1]])
    category = df.wikipedia.sample[which(df.wikipedia.sample$article == f),]$category

    if (!is.null(category) && length(category) > 0) {
      dtm.final[f] = dtm
      articles[f] = category
    }
  }

  ## reset fp
  fp = NULL
}

## merge dataframe
df.merged = as.data.frame(
  names(articles)
)
df.merged$category = unlist(articles[names(articles)], use.names=F)
df.merged$dtm = dtm.final
colnames(df.merged) = c('article', 'category', 'dtm')

##
## create train + test
##
## Note: seed defined to ensure reproducible sample
##
set.seed(123)
sample_size = floor(2/3 * nrow(df.merged))
train = sample(seq_len(nrow(df.merged)), size = sample_size)
df.train = df.merged[train, ]
df.test = df.merged[-train, ]

## term frequency-inverse document frequency
model.tfidf = TfIdf$new()
dtm.tfidf = model.tfidf$fit_transform(df.merged$dtm[[1]])

## generate naive bayes
fit.nb = naive_bayes(
  as.factor(category) ~ df.train$dtm,
  data=df.train,
  laplace = 1
)







## remove time from datetime
df.twitter$timestamp = unlist(lapply(strsplit(
  as.character(df.twitter$timestamp), ' '),
  '[[', 1)
)

## remove 2000-2013
df.twitter = subset(
  df.twitter,
  !grepl('^(200[0-9]{1}|201[0-2]{1}|2013-[0]{1}[1-9]{1}|2013-[1-9]{1}[0-9]{1})-', timestamp)
)
df.ndx = subset(
  df.ndx,
  !grepl('^(200[0-9]{1}|201[0-2]{1}|2013-[0]{1}[1-9]{1}|2013-[1-9]{1}[0-9]{1})-', Date)
)

## convert all words
df.twitter$text = tolower(df.twitter$text)

## apply sentiment analysis
tweets.unnested = df.twitter %>% unnest_tokens(word, text)
tweets = tweets.unnested %>%
  group_by(timestamp) %>%
  mutate(word_count = 1:n()) %>%
  inner_join(get_sentiments('bing')) %>%
  count(timestamp, index = word_count, sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

## tweets: entire plot
nsize = nrow(tweets)
ggplot(tweets, aes(index, sentiment, fill = sentiment > 0)) +
  geom_bar(alpha = 0.5, stat = 'identity', show.legend = FALSE) +
  facet_wrap(~timestamp, ncol = 15, scales = 'free_x')

## save ggplot
ggsave(
  file='visualization/twitter_sentiment.png',
  width = 24,
  height = 12,
  units = 'in'
)

## time series: ndx
ggplot(data = df.ndx) +
  geom_point(aes(Date, as.numeric(High)), color='red') +
  geom_point(aes(Date, as.numeric(Low)), color='blue') +
  labs(x = 'Date', y = 'NDX Price', title = 'NDX Price vs Date') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  'visualization/timeseries-ndx.png',
  width = 16,
  height = 9,
  dpi = 100
)

## remove day suffix
tweets$timestamp = unlist(lapply(
  tweets$timestamp,
  FUN=function(x)(sub('^(201[0-9]{1}-[0-9]{1}[0-9]{1}).*', '\\1', x))
))
df.ndx$Date = unlist(lapply(
  df.ndx$Date,
  FUN=function(x)(sub('^(201[0-9]{1}-[0-9]{1}[0-9]{1}).*', '\\1', x))
))

## aggregate rows
tweets.agg = aggregate(. ~ timestamp, tweets, sum)
names(tweets.agg)[names(tweets.agg) == 'timestamp'] = 'Date'

## merge dataframes
df.ndx.tweets = merge(tweets.agg, df.ndx, all = TRUE)
