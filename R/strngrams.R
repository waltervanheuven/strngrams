#' strngrams
#'
#' Written by Walter van heuven, https://waltervanheuven.net

#' @name bigrams
#' @title bigrams of a character string
#' @author Walter van Heuven
#'
#' @description `bigrams` return the bigrams of a letter string
#'
#' @details Either all possible bigrams or only adjacent bigrams or open bigrams are returned
#'
#' @param the_str character string
#' @param type 'all', 'adjacent', 'open'
#' @param max_distance distance between the letters of the open bigrams. -1 no restriction for the number of open bigrams
#'
#' @return list of characters
#'
#' @examples
#' \dontrun{
#' # only adjacent bigrams
#' bigram("dream")
#' # all possible open bigrams
#' bigram("dream", "all")
#' # max two letters between letters of the open bigram
#' bigram("dream", "open", 3)
#' }
#' @export
bigrams <- function(the_str, type = "adjacent", max_distance = -1) {
  if (class(the_str) != 'character') {
    stop('Character expected')
  }

  type <- tolower(type)
  the_str_len <- stringr::str_length(the_str)
  bigram_list <- c()
  switch(type,
         "all" = {
           # adjacent, non-adjacent and reversed letter combinations in letter string
           for (p1 in 1:the_str_len) {
             for (p2 in 1:the_str_len) {
               if (p1 != p2) {
                 bigram_list <- c(bigram_list,
                                  paste(stringr::str_sub(the_str,p1,p1),
                                        stringr::str_sub(the_str,p2,p2),sep=""))
               }
             }
           }
         },
         "adjacent" = {
           # only adjacent bigrams
           bigram_list <- bigrams(the_str, type = "open", max_distance = 1)
         },
         "open" = {
           # open bigrams; letter combinations are in the correct order

           if (max_distance == -1) {
             # all open bigrams (no restrictions)
             for (p1 in 1:(the_str_len-1)) {
               for (p2 in p1:the_str_len) {
                 if (p1 != p2) {
                   bigram_list <- c(bigram_list,
                                    paste(stringr::str_sub(the_str,p1,p1),
                                          stringr::str_sub(the_str,p2,p2),sep=""))
                 }
               }
             }
           } else {
             # all open bigrams up to a max distance between letter positions
             for (p1 in 1:(the_str_len - 1)) {

               end_pos <- p1 + max_distance
               if (end_pos > the_str_len) {
                 end_pos <- the_str_len
               }

               for (p2 in p1:end_pos) {
                 if (p1 != p2) {
                   bigram_list <- c(bigram_list,
                                    paste(stringr::str_sub(the_str,p1,p1),
                                          stringr::str_sub(the_str,p2,p2),sep=""))
                 }
               }
             }
           }
         }
      )

  return(bigram_list)
}


#' @name ngrams
#' @title ngrams of a string
#' @author Walter van Heuven
#'
#' @description `ngrams` returns letters, bigrams, or trigrams of a string with frequencies added
#'
#' @param the_str string
#' @param type string "monogram", "bigram", "trigram", or integer (number of characters in the ngram)
#' @param frequency frequency of the letter string (word)
#'
#' @return list of ngram, position, type frequency, token frequency
#'
#' @export
ngrams <- function(the_str, type = "bigram", frequency = 1) {
  # monograms: letters
  # bigrams: adjacent letter pairs in the string
  # trigrams: adjacent letter triplets in the string
  if (is.character(type)) {
    t <- switch(type, "monogram" = 1, "bigram" = 2, "trigram" = 3, 2)
  } else {
    t <- type
  }

  # trim and get length
  # the_str <- str_trim(the_str)
  the_str_len <- stringr::str_length(the_str)

  if (the_str_len < t) {
    return( list(ngram = "", position = -1, type_frequency = 0, token_frequency = 0))
  }

  # split string in characters
  ngram_list <- c()
  position_list <- c()
  freq_type_list <- c()
  freq_token_list <- c()

  if (the_str_len > t) {
    last_string_pos <- the_str_len - (t - 1)
  } else {
    last_string_pos <- 1
  }

  for (ngram_start in 1:last_string_pos) {
    ngram_end <- ngram_start + (t - 1)
    ngram_str <- stringr::str_sub(the_str, ngram_start, ngram_end)

    ngram_list <- c(ngram_list, ngram_str)
    position_list <- c(position_list, ngram_start)
    freq_type_list <- c(freq_type_list, 1)
    freq_token_list <- c(freq_token_list, frequency)
  }
  return(
    list(
      ngram = ngram_list,
      position = position_list,
      type_frequency = freq_type_list,
      token_frequency = freq_token_list
    )
  )
}

#' @name get_ngram_frequencies
#' @title ngram frequencies based on lexicon
#' @author Walter van Heuven
#'
#' @description `get_ngram_frequencies` returns the ngrams found in a lexicon and their frequencies
#'
#' @param word_list list of words
#' @param freq_list list of frequencies for each word
#' @param type string "monogram", "bigram" (default), "trigram", or number of characters of ngram
#' @param position_specific ngram is position specific or not: TRUE (default) or FALSE
#' @param progressbar show progress bar TRUE or FALSE
#'
#' @return data frame with ngrams and their frequencies
#'
#' @examples
#' \dontrun{
#' the_file <- system.file("extdata", "test-lexicon.txt", package = "strngrams")
#' db <- read.table(the_file, header = TRUE, fileEncoding = "UTF-8")
#' get_ngram_frequencies(db$word, db$frequency, type = "bigram", position_specific = TRUE)
#' }
#' @export
get_ngram_frequencies <- function(word_list, freq_list, type = "bigram",
                                  position_specific = TRUE,
                                  progressbar = TRUE) {
  # check length of each list is the same
  n_words <- length(word_list)
  if (n_words != length(freq_list)) {
    stop("Word and Frequency list do not have the same length")
  }

  # apply ngram to each word
  if (progressbar == TRUE) {
    ngram_list <- pbapply::pbmapply(word_list, FUN = ngrams, rep(type, n_words), frequency = freq_list)
  } else {
    ngram_list <- mapply(word_list, FUN = ngrams, rep(type, n_words), frequency = freq_list)
  }

  # create data.frame
  s <- as.character(unlist(ngram_list["ngram",]))
  p <- as.numeric(unlist(ngram_list["position",]))
  type_f <- as.numeric(unlist(ngram_list["type_frequency",]))
  token_f <- as.numeric(unlist(ngram_list["token_frequency",]))

  df <- data.frame(ngram = s, pos = p, type_frequency = type_f, token_frequency = token_f)

  # remove empty ngrams
  df <- subset(df,df$ngram != "")

  # aggregate to sum frequencies
  if (position_specific == TRUE) {
    df2 <- stats::aggregate(list(df$type_frequency, df$token_frequency), by = list(df$ngram,df$pos), FUN = sum)
    colnames(df2) <- c("ngram", "pos", "type.frequency", "token.frequency")
  } else {
    df2 <- stats::aggregate(list(df$type_frequency,df$token_frequency),by=list(df$ngram),FUN=sum)
    colnames(df2) <- c("ngram", "type.frequency", "token.frequency")
  }

  return(df2)
}


#' @name ngram_frequency_str
#' @title ngram frequency of a string
#' @author Walter van Heuven
#'
#' @description `ngram_frequency_str` computes the ngram frequency of a single string
#'
#' @param the_str string
#' @param ngram_table data frame with ngram frequencies, use function `get_ngram_frequencies` for this
#' @param type monogram, bigram, or trigram
#' @param position_specific ngrams are position specific or not: TRUE or FALSE
#' @param frequency type or token
#' @param func summed
#'
#' @return summed ngram frequency
#'
#' @export
ngram_frequency_str <- function(the_str, ngram_table, type = "bigram",
                                position_specific = TRUE,
                                frequency = "token",
                                func = "summed") {
  if (is.null(ngram_table)) {
    stop("ngram_table missing")
  }

  if (is.character(type)) {
    t <- switch(type, "monogram" = 1, "bigram" = 2, "trigram" = 3, 2)
  } else {
    t <- type
  }

  the_str_len <- stringr::str_length(the_str)
  if (the_str_len > t) {
    last_string_pos <- the_str_len - (t - 1)
  } else {
    last_string_pos <- 1
  }

  f <- 0
  p <- 1
  lookup <- "ngram"
  for (ngram_start in 1:last_string_pos) {
    ngram_end <- ngram_start + (t - 1)
    ngram_str <- stringr::str_sub(the_str, ngram_start, ngram_end)

    switch(frequency,
           "token" = {
              if (position_specific == TRUE) {
                # check if ngram exists
                if (nrow(ngram_table[ ngram_table[[lookup]] == ngram_str & ngram_table[["pos"]] == p, ]) == 1) {
                  f <- f + ngram_table[ ngram_table[[lookup]] == ngram_str & ngram_table[["pos"]] == p,"token.frequency"]
                }
                  p <- p + 1
              } else {
                  # sum across positions if exists
                  if (nrow(ngram_table[ ngram_table[[lookup]] == ngram_str, ]) == 1) {
                    f <- f + sum(ngram_table[ngram_table[[lookup]] == ngram_str,"token.frequency"])
                  }
              }
           },
           "type" = {
              if (position_specific == TRUE) {
                # check if exists in table
                if (nrow(ngram_table[ngram_table[[lookup]] == ngram_str & ngram_table[["pos"]] == p,]) == 1) {
                  f <- f + ngram_table[ngram_table[[lookup]] == ngram_str & ngram_table[["pos"]] == p,"type.frequency"]
                }
                p <- p + 1
              } else {
                # sum across positions if found
                if (nrow(ngram_table[ngram_table[[lookup]] == ngram_str, ]) == 1) {
                  f <- f + sum(ngram_table[ngram_table[[lookup]] == ngram_str,"type.frequency"])
                }
              }
           }
    )
  }

  # default is summed, calculate mean if func is mean
  switch(func,
         "mean" = {
           f <- f / last_string_pos
         }
  )

  return(f)
}

#' @name ngram_frequency
#' @title ngram frequency of each word in the list
#' @author Walter van Heuven
#'
#' @description `ngram_frequency` returns ngram info for each word (summed frequency) in a list
#'
#' @param word_list list of words
#' @param ngram_table data frame with ngrams
#' @param type monogram, bigram, or trigram
#' @param position_specific TRUE or FALSE
#' @param frequency type or token
#' @param func summed (default) or mean frequency
#' @param progressbar show progress bar TRUE or FALSE
#'
#' @return list of frequencies
#'
#' @export
ngram_frequency <- function(word_list, ngram_table, type = "bigram",
                            position_specific = TRUE,
                            frequency = "token",
                            func = "summed",
                            progressbar = TRUE) {
  if (is.null(ngram_table)) {
    stop("ngram_table missing")
  }
  f <- c()
  word_list <- as.character(word_list)

  if (progressbar == TRUE) {
    f <- as.numeric(pbapply::pblapply(word_list,
                           ngram_frequency_str,
                           ngram_table = ngram_table,
                           type = type,
                           position_specific = position_specific,
                           frequency = frequency,
                           func = func))
  } else {
    f <- as.numeric(lapply(word_list,
                                      ngram_frequency_str,
                                      ngram_table = ngram_table,
                                      type = type,
                                      position_specific = position_specific,
                                      frequency = frequency,
                                      func = func))
  }

  return(f)
}

#' @name anagrams
#' @title anagrams of a string
#' @author Walter van Heuven
#'
#' @description `anagrams` returns all possible anagrams of the string
#'
#' @details Note that this function uses a brute force method so use only short strings (<12)
#' @param the_str string
#' @param wordList list of words
#' @param progressbar show progress bar TRUE or FALSE
#' @return vector of all possible anagrams
#'
#' @examples
#' \dontrun{
#' # return all anagrams that are words
#' anagrams("dream", vwr::english.words)
#'
#' # return all anagram letter strings, if none is found return NULL
#' anagrams("dream")
#' }
#' @export
anagrams <- function(the_str, wordList = NULL, progressbar = TRUE) {
  if (is.null(wordList)) {
    # create all possible permutations of the_str!
    # no check if these are words
    # avoid using long strings as the number of permutations explode, n!
    str_letters <- unlist(strsplit(the_str, ""))
    the_permutations <- combinat::permn(str_letters)
    the_list <- unlist(lapply(the_permutations, FUN=paste, sep="", collapse=""))
    the_list <- unique(the_list)

  } else {
    # use wordList to find word anagrams
    if (progressbar == TRUE) {
      the_list <- wordList[unlist(pbapply::pblapply(wordList, FUN=are_anagrams, w1=the_str))]
    } else {
      the_list <- wordList[unlist(lapply(wordList, FUN=are_anagrams, w1=the_str))]
    }
  }
  if (length(the_list) == 0) {
    the_list <- NULL
  }
  return(the_list)
}

#' @name are_anagrams
#' @title returns TRUE if both strings are anagrams and not the same
#' @author Walter van Heuven
#'
#' @param w1 string 1
#' @param w2 string 2
#'
#' @return TRUE or FALSE
#'
#' @export
are_anagrams <- function(w1, w2) {
  if (stringr::str_length(w1) == stringr::str_length(w2)) {
    if (w1 != w2) {
      w1_letters <- unlist(strsplit(w1, ""))
      w2_letters <- unlist(strsplit(w2, ""))
      for (l in w1_letters) {
        w2_letters <- w2_letters[-match(l, w2_letters)]
      }
      if (length(w2_letters) == 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

#' @name sbf_rank
#' @title Summed Bigram Frequency Rank
#' @author Walter van Heuven
#'
#' @description `sbf_rank` returns the Summed Bigram Frequency (SBF) Rank
#'
#' @details Based on definition found in Novick & Sherman (2004).
#' @references \url{https://link.springer.com/article/10.3758/BF03195587}
#'
#' @param the_str string
#' @param bigram_table data frame with ngrams
#' @param top12 TRUE or FALSE
#' @param method method to be used, e.g. Novick (default)
#'
#' @return list with SBF, SBF rank, and anagrams with SBF >= SBF the_str
#'
#' @export
sbf_rank <- function(the_str, bigram_table, top12 = FALSE, method = "Novick") {
  bigrams <- bigrams(the_str, type = "adjacent")
  the_anagrams <- anagrams(the_str)

  switch(method,
         "Novick" = {
           # Method based on Novick & Sherman (2004)

           bf <- c() # list of frequencies of each bigram
           p <- 1 # position
           for (b in bigrams) {
             # frequency bigram
             frequency <- bigram_table[bigram_table$ngram == b & bigram_table$pos == p, "type.frequency"]
             bf <- c(bf, frequency)
             p <- p + 1
           }

           # summed bigram frequency of the_str
           #sbf_the_str <- str_summed_ngram(the_str, bigram_table, type = "bigram", frequency = "type")
           sbf_the_str <- ngram_frequency_str(the_str, bigram_table, type = "bigram", frequency = "type")

           # data.frame with anagrams
           df <- data.frame(anagrams = the_anagrams, sbf = 0)
           df$sbf <- ngram_frequency(df$anagrams, bigram_table, type = "bigram", frequency = "type")

           # NA to zero
           df[is.na(df)] <- 0
           df <- df[order(-df$sbf),]
           # only anagrams with sbf >= sbf(the_str)
           rank_list <- subset(df, df$sbf >= sbf_the_str)
           sbf_rank <- nrow(rank_list)

           if (top12 == FALSE) {
             the_list <- rank_list
           } else {
             the_list <- utils::head(df, 12)
           }
         },
         "Gilhooly" = {
           #  NOT YET WORKING PROPERLY

           # method based on Gilhooly (1978)
           # requires type frequencies
           sbf_the_str <- 0

           bf <- c() # list of frequencies of each bigram
           p <- 1 # position
           for (b in bigrams) {
             # frequency bigram
             frequency <- bigram_table[bigram_table$ngram == b & bigram_table$pos == p, "type.frequency"]
             # store frequency of correct
             bf <- c(bf, frequency)
             # summed bigram frequency
             sbf_the_str <- sbf_the_str + frequency

             p <- p + 1
           }

           # data.frame with anagrams
           df <- data.frame(anagrams = the_anagrams, sbf = 0)
           df$sbf <-
             ngram_frequency(df$anagrams,
                        bigram_table,
                        type = "bigram",
                        frequency = "type")

           # keep ...
           for (b in bigrams) {

           }

           rank_list = df
           the_list = df
         })

  return_df <- list(
                    bigrams = bigrams,
                    frequencies = bf,
                    sbf = sbf_the_str,
                    sbf_rank = nrow(rank_list),
                    anagrams = the_list
  )

  return(return_df)
}

#' @name gtzero
#' @title gtzero
#' @author Walter van Heuven
#'
#' @description `gtzero` returns the number of bigrams in letter string that exist in the bigram_table.
#'
#' @details Based on definition of Mendelsohn (1976),
#' @references \url{https://link.springer.com/article/10.3758/BF03213228}
#'
#' @param the_str letter string
#' @param bigram_table data frame with ngrams
#'
#' @return number of bigrams
#'
#' @export
gtzero <- function(the_str, bigram_table) {
  anagrams <- anagrams(the_str)
  n <- length(anagrams)

  bt <- get_ngram_frequencies(anagrams, rep(1,n), type = "bigram", position_specific = T)

  # find the bigrams that are in the bigram_table
  # match on bigram and position
  found <- merge(bt[, c("ngram","pos")], bigram_table, by = c("ngram","pos"))
  gtz <- nrow(found)

  return(gtz)
}


