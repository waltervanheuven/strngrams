# strngrams

[R](https://www.r-project.org) package with functions to extract ngrams (e.g., letters, bigrams, trigrams), create anagrams, and calculate summed/mean ngram type and token frequencies of letter strings.

## Installation

```R
library(devtools)
devtools::install_github("waltervanheuven/strngrams")

library(strngrams)
```

## Usage

### get bigrams

```R
bigrams("DREAM")
# [1] "DR" "RE" "EA" "AM"
```

### open bigrams

```R
bigrams("DREAM", "open")
# [1] "DR" "DE" "DA" "DM" "RE" "RA" "RM" "EA" "EM" "AM"
```

### ngrams

```R
ngrams("DREAM", "trigram")
#$ngram
#[1] "DRE" "REA" "EAM"
#
#$position
#[1] 1 2 3
#
#$type_frequency
#[1] 1 1 1
#
#$token_frequency
#[1] 1 1 1

ngrams("DREAM", 4)
#$ngram
#[1] "DREA" "REAM"
#
#$position
#[1] 1 2
#
$type_frequency
#[1] 1 1
#
#$token_frequency
#[1] 1 1
```

### Anagrams

```R
anagrams("DREAM")
#  [1] "DREAM" "DREMA" "DRMEA" "DMREA" "MDREA" "MDRAE" "DMRAE" "DRMAE" "DRAME" "DRAEM" "DAREM"
# [12] "DARME" "DAMRE" "DMARE" "MDARE" "MADRE" "AMDRE" "ADMRE" "ADRME" "ADREM" "ADERM" "ADEMR"
# [23] "ADMER" "AMDER" "MADER" "MDAER" "DMAER" "DAMER" "DAEMR" "DAERM" "DEARM" "DEAMR" "DEMAR"
# [34] "DMEAR" "MDEAR" "MDERA" "DMERA" "DEMRA" "DERMA" "DERAM" "EDRAM" "EDRMA" "EDMRA" "EMDRA"
# [45] "MEDRA" "MEDAR" "EMDAR" "EDMAR" "EDAMR" "EDARM" "EADRM" "EADMR" "EAMDR" "EMADR" "MEADR"
# [56] "MAEDR" "AMEDR" "AEMDR" "AEDMR" "AEDRM" "AERDM" "AERMD" "AEMRD" "AMERD" "MAERD" "MEARD"
# [67] "EMARD" "EAMRD" "EARMD" "EARDM" "ERADM" "ERAMD" "ERMAD" "EMRAD" "MERAD" "MERDA" "EMRDA"
# [78] "ERMDA" "ERDMA" "ERDAM" "REDAM" "REDMA" "REMDA" "RMEDA" "MREDA" "MREAD" "RMEAD" "REMAD"
# [89] "REAMD" "READM" "RAEDM" "RAEMD" "RAMED" "RMAED" "MRAED" "MARED" "AMRED" "ARMED" "AREMD"
#[100] "AREDM" "ARDEM" "ARDME" "ARMDE" "AMRDE" "MARDE" "MRADE" "RMADE" "RAMDE" "RADME" "RADEM"
#[111] "RDAEM" "RDAME" "RDMAE" "RMDAE" "MRDAE" "MRDEA" "RMDEA" "RDMEA" "RDEMA" "RDEAM"
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
the_file <- system.file("extdata", "test-lexicon.txt", package = "strngrams")
db <- read.table(the_file, header = TRUE, fileEncoding = "UTF-8")
print(db)

# extract position specific bigrams and their frequencies from a lexicon
#
# returned data frame contains both type and token frequencies for each bigram
#
df_bigrams <- get_ngram_frequencies(db$word, db$frequency, type = "bigram", position_specific = TRUE)
print(df_bigrams)

#   bigram pos type.frequency token.frequency
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

# calculate summed positional bigram frequencies and add those to db with words
db$sum.bigram.freq.token <- ngram_frequency(db$word, df_bigrams, type = "bigram", position_specific = TRUE, frequency = "token", func = "summed")

# calculate mean positional bigram frequencies and add those to db with words
db$mean.bigram.freq.token <- ngram_frequency(db$word, df_bigrams, type = "bigram", position_specific = TRUE, frequency = "token", func = "mean")

# note that functions above can take a long time when using a large lexicon

# show lexicon with type and token bigram frequencies
print(db)

#  word frequency sum.bigram.freq.token mean.bigram.freq.token
#1 bank        10                    38              12.666667
#2 bobo         8                     8               2.666667
#3 bald         2                    22               7.333333
#4 baop         6                    30              10.000000

# save to tab-delimited file
write.table(db, col.names = T, row.names = F, file="test-lexicon_BF.txt", quote = FALSE, fileEncoding="UTF-8", sep="\t")

```
