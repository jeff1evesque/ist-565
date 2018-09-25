##
## load_corpus.R, loads sourcefile(s) into vectorized collection.
##
## Note: this script requires the following packages:
##
##     - text2vect
##     - data.table
##
load_corpus = function(source, remove=FALSE, type='txt', subset='') {
  ##
  ## load source(s) into dataframe
  ##
  ## @list.files, runs on the current directory
  ##
  ## Note: https://cran.r-project.org/web/packages/text2vec/vignettes/files-multicore.html
  ##
  if (file_test('-d', source)) {
    ## generate path of all files
    files = list.files(
      source,
      pattern = '.txt',
      full.names = TRUE,
      all.files = TRUE
    )

    ## ensure nonempty content
    f = file.info(files)
    nonempty_files = rownames(f[which(f$size > 0),])

    ##
    ## subset result: remove if not in sample dataset
    ##
    if (length(subset) > 0) {
      validate = paste0(subset, '.txt')
      original = basename(nonempty_files)
      basepath = dirname(nonempty_files[1])
      nonempty_files = paste0(basepath, '/', Reduce(intersect, list(original, validate)))
    }

    ## iterator over files
    it_files = ifiles(nonempty_files)

    ## iterator over tokens on files iterator
    it_tokens = itoken(
        it_files,
        preprocessor = tolower,
        tokenizer = word_tokenizer,
        progressbar = FALSE
    )

    ## create vocabulary
    vocab = create_vocabulary(it_tokens)
    vectorizer = vocab_vectorizer(vocab)

    ## term frequency-inverse document frequency
    dtm = create_dtm(it_tokens, vectorizer)
    model.tfidf = TfIdf$new()
    dtm.tfidf = model.tfidf$fit_transform(dtm)

    return(dtm.tfidf)
  }
  else {
    return(FALSE)
  }
}