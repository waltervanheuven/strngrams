# strngrams

[R](https://www.r-project.org) package with functions to extract ngrams (e.g., letters, bigrams, trigrams), create anagrams, and calculate summed/mean ngram type and token frequencies of letter strings.

## Installation

```R
library(devtools)
devtools::install_github("waltervanheuven/strngrams")
library(strngrams)
```

## Usage

### bigrams (adjacent only)

```R
bigrams("dream")
# [1] "dr" "re" "ea" "am"
```

### open bigrams

```R
bigrams("dream", "open")
# [1] "dr" "de" "da" "dm" "re" "ra" "rm" "ea" "em" "am"

# max two letters between the letters of the open bigram
bigrams("dream", "open", max_distance=3)
# [1] "dr" "de" "da" "re" "ra" "rm" "ea" "em" "am"
```

### ngrams

```R
ngrams("dream", "trigram")
#$ngram
#[1] "dre" "rea" "eam"
#
#$position
#[1] 1 2 3
#
#$type_frequency
#[1] 1 1 1
#
#$token_frequency
#[1] 1 1 1

ngrams("dream", 4)
#$ngram
#[1] "drea" "ream"
#
#$position
#[1] 1 2
#
#$type_frequency
#[1] 1 1
#
#$token_frequency
#[1] 1 1
```

### Anagrams

```R
library(vwr)

anagrams("dream", vwr::english.words)
# [1] "armed"

anagrams("silence", vwr::english.words)
# [1] "selenic" "license"
```

### Bigram frequencies of words in lexicon

Example to calculate type and token frequencies of each word in a small lexicon
based on the word frequencies provide in the lexicon.

Lexicon should have two columns and separated by a space or tab. Required headers: word and frequency. Example lexicon:

```txt
word  frequency
bank  10
bobo  8
bald  2
baop  6
```

Script example to load lexicon, calculate bigram frequencies , and save results file.

```R
# test lexicon with 4 words
#

the_file <- system.file("extdata", "test-lexicon.txt", package = "strngrams")
db <- read.table(the_file, header = TRUE, fileEncoding = "UTF-8")
print(db)

# extract position specific bigrams and their frequencies from a lexicon
#
# returned data frame contains both type and token frequencies for each bigram
#
df_bigrams <- get_ngram_frequencies(db$word, db$frequency, type = "bigram", position_specific = TRUE)
print(df_bigrams)

#   ngram pos type.frequency token.frequency
#1      ba   1              3              18
#2      bo   1              1               8
#3      al   2              1               2
#4      an   2              1              10
#5      ao   2              1               6
#6      ob   2              1               8
#7      bo   3              1               8
#8      ld   3              1               2
#9      nk   3              1              10
#10     op   3              1               6

# summed bigram frequency (SBF)
#
# type base
db$sbf.type <- ngram_frequency(db$word, df_bigrams, type="bigram", position_specific=TRUE, frequency = "type", func = "summed")

# token  based
db$sbf.token <- ngram_frequency(db$word, df_bigrams, type="bigram", position_specific=TRUE, frequency="token", func="summed")

# mean bigram frequency
#
# type
db$mbf.type <- ngram_frequency(db$word, df_bigrams, type="bigram", position_specific=TRUE, frequency="type", func="mean")

db$mbf.token <- ngram_frequency(db$word, df_bigrams, type="bigram", position_specific=TRUE, frequency="token", func="mean")

# show lexicon with type and token bigram frequencies
print(db)

#  word frequency sbf.type sbf.token mbf.type mbf.token
#1 bank        10        5        38 1.666667 12.666667
#2 bobo         8        3         8 1.000000  2.666667
#3 bald         2        5        22 1.666667  7.333333
#4 baop         6        5        30 1.666667 10.000000

# save to tab-delimited file
write.table(db, col.names = T, row.names = F, file="test-lexicon_BF.txt", quote = FALSE, fileEncoding="UTF-8", sep="\t")

```

Summed bigram frequency (SBF) of 66,330 English words

```R
library(vwr)

# 66,330 English words from the CELEX lexical database
db <- data.frame(word = vwr::english.words)

# get bigrams, ignore token frequency (token frequency = 1 for all words)
df_bigrams <- get_ngram_frequencies(db$word, rep(1,nrow(db)), type="bigram", position_specific=TRUE)

# type based SBF of each word
db$sbf.type <- ngram_frequency(db$word, df_bigrams, type="bigram", position_specific=TRUE, frequency="type", func="summed")

# top10 words based on SBF
head(db[order(-db$sbf.type),], n=10)

#                word sbf.type
#30656  interestingly    11871
#12094     contesting    11647
#30655    interesting    11542
#12426    cooperating    11213
#11351     concerting    11166
#12080     contenting    11117
#30654 interestedness    11040
#12321     converting    10992
#15782   destinations    10953
#11651     congesting    10931
```

English letter frequency (type)

```R

library(vwr)

# 66,330 English words from the CELEX lexical database
db <- data.frame(word = vwr::english.words)

# get letter frequency based on types 
lf <- get_ngram_frequencies(db$word, rep(1,nrow(db)), type="monogram", position_specific=FALSE)

alphabeth <- unlist(strsplit("abcdefghijklmnopqrstuvwxyz",""))
lf[lf$ngram %in% alphabeth == TRUE, c("ngram", "type.frequency")]

#   ngram type.frequency
#5      a          41853
#6      b          10591
#7      c          22583
#8      d          21189
#9      e          63385
#10     f           7850
#11     g          16686
#12     h          12385
#13     i          48377
#14     j           1070
#15     k           5084
#16     l          29331
#17     m          14723
#18     n          38882
#19     o          33296
#20     p          16224
#21     q           1086
#22     r          39367
#23     s          50851
#24     t          38222
#25     u          18460
#26     v           5636
#27     w           4940
#28     x           1487
#29     y           9071
#30     z           2118

```

