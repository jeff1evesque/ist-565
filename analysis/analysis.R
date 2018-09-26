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
  'tm',
  'randomForest',
  'rpart'
))

## create dataframes
df.wikipedia = load_data(paste0(cwd, '/data/wikipedia'), remove=TRUE, type='json')

## article list: rank, views, article, category
wikipedia.sample = fromJSON('2016-08-01--sample-train.json')
wikipedia.list = as.data.frame(wikipedia.sample$items$articles)

## generate corpus
corpus_split = load_corpus(paste0(cwd, '/data/wikipedia/articles'), subset=wikipedia.list$article)
df.merged = as.data.frame(as.matrix(corpus_split))

## append article column + category column
df.merged$article_name = lapply(dimnames(corpus_split)[1], FUN = function(x) { gsub("_[0-9]+$", '', x) })

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
#  df.merged
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

##
## generate naive bayes
##
nb.start = Sys.time()
fit.nb = naive_bayes(
  as.factor(X.category) ~ .,
  data=df.train,
  laplace = 1
)
nb.end = Sys.time()

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
## random forest: instead of using 'method=class' like rpart, 'as.factor'
##     is implemented directly on the formula.
##
rf.start = Sys.time()
fit.rf = randomForest(
  as.factor(X.category) ~ .,
  data = df.train,
  ntree = 1
)
rf.end = Sys.time()

##
## random forest report
##
sink('visualization/rf-wikipedia.txt')
cat('===========================================================\n')
cat('random forest model: 30 trees\n')
cat('===========================================================\n')
fit.rf
cat('\n\n')
cat('===========================================================\n')
cat('prediction: \n')
cat('===========================================================\n')
predict(fit.rf, df.test)
sink()

##
## decision tree
##
tree.start = Sys.time()
fit.tree = rpart(
  as.factor(X.category) ~ .,
  data = df.train,
  method = 'class'
)
tree.end = Sys.time()

tree.class.start = Sys.time()
fit.tree.class = predict(fit.tree, df.test, type = 'class')
tree.class.end = Sys.time()

## visualize default tree
png('tree-wikipedia.png', width=10, height=5, units='in', res=1400)
rpart.plot(fit.tree)
dev.off()

## decision tree summary
sink('visualization/tree-wikipedia.txt')
cat('===========================================================\n')
cat(' Note: the "root node error" is the error rate for a single\n')
cat(' node tree, if the tree was pruned to node 1. It is useful\n')
cat(' when comparing different decision tree models. measures of\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
printcp(fit.tree)
cat('\n\n')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on training sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.tree.table = table(predict(fit.tree, type='class'), df.test$label)
1-sum(diag(fit.tree.table))/sum(fit.tree.table)
cat('\n\n')
cat('===========================================================\n')
cat(' cross validation performance \n')
cat('===========================================================\n')
xpred.rpart(fit.tree, xval=3)
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (class) \n')
cat('===========================================================\n')
fit.tree.class
cat('\n\n')
cat('===========================================================\n')
cat(' performance (minutes) \n')
cat('===========================================================\n')
paste('fitting tree: ', tree.end - tree.start)
paste('predicting class: ', tree.class.end - tree.class.start)
cat('===========================================================\n')
sink()
