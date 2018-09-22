##
## score_text.R, use 'afinn' package to score text.
##
score_word = function(x) {
  word_bool_vec = get_sentiments('afinn')$word==x
  score = get_sentiments('afinn')$score[word_bool_vec]
  return (score)
}

score_tweet = function(sentence) {
  words = unlist(strsplit(sentence, ' '))
  words = as.vector(words)
  scores = sapply(words, score_word)
  scores = unlist(scores)
  return(sum(scores))
}
