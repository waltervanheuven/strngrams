# strngram

[R](https://www.r-project.org) package with functions to extract ngrams (e.g., letters, bigrams, trigrams), create anagrams, and calculate summed/mean ngram type and token frequencies of letter strings.

## Installation

```R
install.packages("")
```


## Usage

### get bigrams

```R
str_bigrams("DREAM")
# [1] "DR" "RE" "EA" "AM"
```

### open bigrams

```R
str_bigrams("DREAM", "open")
# [1] "DR" "DE" "DA" "DM" "RE" "RA" "RM" "EA" "EM" "AM"
```

### ngram with position and frequency information

```R
str_ngrams("DREAM")
#$ngram
#[1] "DR" "RE" "EA" "AM"
#
#$position
#[1] 1 2 3 4
#
#$type_frequency
#[1] 1 1 1 1
#
#$token_frequency
#[1] 1 1 1 1
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

Script example to load lexicon, bigram frequencies , and saving file.

```R
# test lexicon with 4 words
db <- read.table("wordlists/test-lexicon.txt", header = TRUE, fileEncoding = "UTF-8")
print(db)

# get position specific bigram table based on lexicon
#
# table contains both type nd token frequencies for each bigram
#
bigram_table <- str_calc_ngrams(db$word, db$frequency, type = "bigram", position_specific = TRUE)
print(bigram_table)

# calculate summed positional bigram frequencies and add those to db with words
db$sum.bigram.freq.token <- str_ngram_info(db$word, bigram_table, type = "bigram", position_specific = TRUE, frequency = "token", func = "summed")

# calculate mean positional bigram frequencies and add those to db with words
db$mean.bigram.freq.token <- str_ngram_info(db$word, bigram_table, type = "bigram", position_specific = TRUE, frequency = "token", func = "mean")

# note that functions above can take a long time when using a large lexicon

# show lexicon with type and token bigram frequencies
print(db)

# save to tab-delimited file
write.table(db, col.names = T, row.names = F, file="tmp/db.txt", quote = FALSE, fileEncoding="UTF-8", sep="\t")

```
