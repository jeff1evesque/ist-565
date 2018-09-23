##
## exploratory.R, load + exploratory analysis:
##
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
load_package(c('data.table', 'RJSONIO', 'tidytext', 'tidyverse', 'gtools'))

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
df.ixic = subset(
  df.ixic,
  !grepl('^(200[0-9]{1}|201[0-2]{1}|2013-[0]{1}[1-9]{1}|2013-[1-9]{1}[0-9]{1})-', Date)
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

## time series: ixic
ggplot(data = df.ixic) +
  geom_point(aes(Date, as.numeric(Open)), color='red') +
  geom_point(aes(Date, as.numeric(Close)), color='blue') +
  labs(x = 'Date', y = 'IXIC Price', title = 'IXIC Price vs Date') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  'visualization/timeseries-ixic.png',
  width = 16,
  height = 9,
  dpi = 100
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
df.ixic$Date = unlist(lapply(
  df.ixic$Date,
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
df.ixic.tweets = merge(tweets.agg, df.ixic, all = TRUE)
