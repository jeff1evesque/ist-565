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
load_package(c('data.table', 'RJSONIO', 'tidytext', 'tidyverse', 'gtools'))

## create dataframes
df.ixic = load_data(paste0(cwd, '/data/nasdaq/^ixic.csv'), remove=TRUE, type='csv')

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
df.ixic$day = weekdays(as.Date(df.ixic$Date,'%d/%m/%Y'))
df.ixic = df.ixic[-c(1,nrow(df.ixic)),]
