##
## wikipedia.R, classification analysis:
##
##     - wikipedia
##

## set project cwd: only execute in RStudio
if (nzchar(Sys.getenv('RSTUDIO_USER_IDENTITY'))) {
  cwd <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  setwd(cwd)
}

## create ignored directories
dir.create(file.path(cwd, 'visualization'), showWarnings = FALSE)

## utility functions
devtools::install_local(paste0(cwd, '/packages/customUtility'))
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
  'naivebayes',
  'FSelector',
  'tm'
))

## create dataframes
df.wikipedia = load_data(paste0(cwd, '/data/wikipedia'), remove=TRUE, type='json')

## article list: rank, views, article, category
wikipedia.sample = fromJSON('2016-08-01--sample-train.json')
wikipedia.list = as.data.frame(wikipedia.sample$items$articles)

## generate corpus
corpus_split = load_corpus(paste0(cwd, '/data/wikipedia/articles'), subset=wikipedia.list$article)
df.merged = as.data.frame(as.matrix(corpus_split))

## aggregate articles
df.agg.start = Sys.time()
df.merged = aggregate(
  df.merged[, -c(
    which(names(df.merged)=='article_name')
  )],
  by = c(df.merged$article_name),
  na.rm = TRUE,
  na.action = 0,
  FUN = sum
)
df.agg.end = Sys.time()

## store article names
articles.nodupe = unique(lapply(dimnames(corpus_split)[1], FUN = function(x) { gsub('_[0-9]+$', '', x) })[[1]])
row.names(articles.nodupe)

## lookup + append category
X.category = lapply(articles.nodupe, FUN = function(x) {
  wikipedia.list[
    which(wikipedia.list$article == gsub('.txt$', '', x)),
    grep('^category$', colnames(wikipedia.list))
  ]
})
df.merged$X.category = X.category

## ensure dataframe with vectors not lists
df.merged = as.data.frame(lapply(df.merged, unlist))

## reduce feature set
#feature.set = chi.squared(
#  as.factor(X.category) ~ .,
#  df.merged[-c(which(names(df.merged)=='article_name')),]
#)

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

## generate naive bayes
fit.nb = naive_bayes(
  as.factor(X.category) ~ .,
  data=df.train,
  laplace = 1
)

## predict against test
nb.pred = predict(fit.nb, df.test)
fit.nb.table = table(nb.pred, df.test$category)
nb.error = 1-sum(diag(fit.nb.table))/sum(fit.nb.table)

##
## naive bayes report
##
sink('visualization/nb.txt')
cat('===========================================================\n')
cat('naive bayes model: \n')
cat('===========================================================\n')
fit.nb
cat('\n\n')
cat('===========================================================\n')
cat('prediction: \n')
cat('===========================================================\n')
nb.pred
cat('\n\n')
cat('===========================================================\n')
cat('confusion matrix:\n')
cat('===========================================================\n')
fit.nb.table
cat('\n\n')
cat('===========================================================\n')
cat('resubstitution error:\n')
cat('===========================================================\n')
nb.error
sink()

##
## plot naive bayes
##
png('visualization/nb-wikipedia.png', width=10, height=5, units='in', res=1400)
plot(fit.nb, main='Naive Bayes: categorize articles')
dev.off()
