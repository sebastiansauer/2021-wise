my_pckgs <- c("tidyverse", "tidytext", "janeaustenr", "stringr", "wordcloud", "gutenbergr", "forcats", "textdata", "scales")

install.packages(my_pckgs)


# Sentiment-Lexikons herunterladen:

library(tidytext)

get_sentiments("afinn")
get_sentiments("loughran")
get_sentiments("nrc")
get_sentiments("bing")

