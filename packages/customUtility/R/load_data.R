##
## load_csv.R, loads sourefile(s) into dataframe.
##
## Note: this script requires the following packages:
##
##     - data.table
##     - RJSONIO
##
load_data = function(source, remove=FALSE, type='csv') {
  ##
  ## load source(s) into dataframe
  ##
  ## @list.files, runs on the current directory
  ##
  if (file_test('-f', source)) {
    files = list.files(pattern=paste('*.', type, sep=''))

    if (type == 'csv') {
      df = do.call(rbind, lapply(files, fread))
    } else if (type == 'json') {
      df = do.call(rbind, lapply(files, fromJSON))
    }
  } else if (file_test('-d', source)) {
    if (type == 'csv') {
      df = read.csv(source, header = TRUE)
    } else if (type == 'json') {
      df = fromJSON(source)
    }
  }

  ## optionally remove NA rows
  if (remove) {
      df = df[complete.cases(df),]
  }

  ## return dataframe
  return(df)
}
