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

## utility functions
devtools::install_local(paste(cwd, sep='', '/packages/customUtility'))
library('customUtility')

## load packages
load_package(c('data.table', 'RJSONIO'))

## create dataframes
df.wikipedia = load_data(paste0(cwd, '/data/wikipedia'), remove=TRUE, type='json')
df.twitter = load_data(paste0(cwd, '/data/twitter'), remove=TRUE, type='json')
df.ixic = load_data(paste0(cwd, '/data/nasdaq/^ixic.csv'), remove=TRUE, type='csv')
df.ndx = load_data(paste0(cwd, '/data/nasdaq/^ndx.csv'), remove=TRUE, type='csv')
