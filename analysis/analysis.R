##
## arima.R, ARIMA analysis on wikipedia traffic:
##
##     - https://www.dropbox.com/s/x14f3bg8flej1n7/train-wikipedia.csv?dl=1
##     - https://www.dropbox.com/s/o2df10dnyt3bg02/test-wikipedia.csv?dl=1
##

## set project cwd: only execute in RStudio
if (nzchar(Sys.getenv('RSTUDIO_USER_IDENTITY'))) {
  cwd <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  setwd(cwd)
}

## utility functions
devtools::install_local(paste(cwd, sep='', '/packages/customUtility'))
library('customUtility')

## load packages
load_package(c('data.table', 'RJSONIO'))

## create dataframes
df.wikipedia = load_data('data/wikipedia', remove=TRUE, type='json')
df.twitter = load_data('data/twitter', remove=TRUE, type='json')
df.ixic = load_data('data/nasdaq/^ixic.csv', remove=TRUE, type='csv')
df.ndx = load_data('data/nasdaq/^ndx.csv', remove=TRUE, type='csv')
