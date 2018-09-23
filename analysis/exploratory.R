##
## exploratory.R, load + exploratory analysis:
##
##     - twitter
##     - nasdaq (ixic)
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
load_package(c('data.table', 'RJSONIO', 'tidytext', 'tidyverse', 'gtools', 'stringr', 'naivebayes', 'stats'))

## create dataframes
df.ixic = load_data(paste0(cwd, '/data/nasdaq/^ixic.csv'), remove=TRUE, type='csv')

## reorder date: allows 'weekday' implementation
reorder_date = function(x) {
  original = str_split(x, '-')
  return(
    paste0(
      original[[1]][3],
      '-',
      original[[1]][2],
      '-',
      original[[1]][1]
    )
  )
}

df.ixic$Date = lapply(
  df.ixic$Date,
  FUN=reorder_date
)

## time series: ixic
ggplot(data = df.ixic) +
  geom_point(aes(Date, as.numeric(High)), color='red') +
  geom_point(aes(Date, as.numeric(Low)), color='blue') +
  labs(x = 'Date', y = 'ixic Price', title = 'ixic Price vs Date') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  'visualization/timeseries-ixic.png',
  width = 16,
  height = 9,
  dpi = 100
)

## day of week
df.ixic$day = weekdays(as.Date(unlist(df.ixic$Date),'%d-%m-%Y'))
df.ixic = df.ixic[-c(1,nrow(df.ixic)),]
df.ixic = df.ixic[, -which(names(df.ixic) == 'Date')]

## train + test
set.seed(123)
smp_size = floor(0.75 * nrow(df.ixic))
train_ind = sample(seq_len(nrow(df.ixic)), size = smp_size)

train = df.ixic[train_ind, ]
test = df.ixic[-train_ind, ]

## train naive bayes
fit.nb = naive_bayes(
  day ~ .,
  data=train,
  laplace = 1
)

## predict against test
nb.pred = predict(fit.nb, test)
fit.nb.table = table(nb.pred, test$day)
1-sum(diag(fit.nb.table))/sum(fit.nb.table)
