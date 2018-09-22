##
## analysis.R, load + generate analysis:
##
##     - wikipedia
##     - twitter
##     - nasdaq (ixic)
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
##
load_package(c('data.table', 'RJSONIO', 'tidytext', 'tidyverse'))

## create dataframes
df.wikipedia = load_data(paste0(cwd, '/data/wikipedia'), remove=TRUE, type='json')
df.twitter = load_data(paste0(cwd, '/data/twitter'), remove=TRUE, type='json')
df.ixic = load_data(paste0(cwd, '/data/nasdaq/^ixic.csv'), remove=TRUE, type='csv')
df.ndx = load_data(paste0(cwd, '/data/nasdaq/^ndx.csv'), remove=TRUE, type='csv')

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

## convert all words
df.twitter$text = tolower(df.twitter$text)

## apply sentiment analysis
unnested_tweet = df.twitter %>% unnest_tokens(word, text)
tweet_sentiments = unnested_tweet %>%
  group_by(timestamp) %>%
  mutate(word_count = 1:n()) %>%
  inner_join(get_sentiments('bing')) %>%
  count(timestamp, index = word_count, sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

## 2013 subset ggplot
twitter_subset = c()
years = c('2014', '2015', '2016', '2017', '2018')

for (year in years) {
  ## generate ggplot
  ggplot(tweet_sentiments, aes(index, sentiment, fill = sentiment > 0)) +
    geom_bar(alpha = 0.5, stat = 'identity', show.legend = FALSE) +
    facet_wrap(~timestamp, ncol = 7, scales = 'free_x')

  ## save ggplot
  ggsave(file=paste0('visualization/twitter_sentiment--', year, '.png'), width = 14, height = 10, units = 'in')
}
