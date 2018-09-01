##
## load_csv.R, loads sourefile(s) into dataframe.
##
## Note: this script requires the 'data.table' package.
##
load_df = function(source, remove=FALSE) {
  ##
  ## load source(s) into dataframe
  ##
  ## @list.files, runs on the current directory
  ##
  if (is.vector(source) {
    files = list.files(pattern='*.csv')
    df = do.call(rbind, lapply(files, fread)
  } else {
    df = read.csv(source, header = TRUE)
  }

  ## optionally remove NA rows
  if (remove) {
      df = df[complete.cases(df),]
  }

  ## return dataframe
  return(df)
}
