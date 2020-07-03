# strngrams.R
#
#' @author Walter van Heuven, \email{walter.vaheuven@@nottingham.ac.uk}
#
# Set of functions to calculate anagrams, bigrams, and trigrams from letter strings

#' @name str_bigrams
#'
#' Returns bigrams from a string. Either all possible bigrams are returned, or only adjacent bigrams
#' or open bigrams.
#'
#' @param the_str a string
#' @param type 'all', 'adjacent', 'open'
#' @param max_distance distance between the letters of the open bigrams. -1 no restriction for the number of open bigrams
#'
#' @return list of characters
#'
str_bigrams <- function(the_str, type = "adjacent", max_distance = -1) {
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
           bigram_list <- str_bigrams(the_str, type = "open", max_distance = 1)
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

#' str_ngrams returns monograms, bigrams, or trigrams of a string with frequency information
#'
#' @name str_ngrams
#'
#' @param the_str string of characters
#' @param type monogram, bigram, or trigram
#' @param frequency frequency of the the_str
#'
#' @return list of ngram, position, type frequency, token frequency
#'
str_ngrams <- function(the_str, type = "bigram", frequency = 1) {
  # monograms: letters
  # bigrams: adjacent letter pairs in the string
  # trigrams: adjacent letter triplets in the string
  t <- switch(type, "monogram" = 1, "bigram" = 2, "trigram" = 3)

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

#' Caclulate ngrams based strings in a list
#'
#' @name str_calc_ngrams
#'
#' @param word_list list of words
#' @param frequency_list list of frequencies for each word
#' @param type monogram, bigram, or trigram
#' @param position_specific TRUE or FALSE
#'
#' @return data.frame with a table of ngrams
#'
str_calc_ngrams <- function(word_list, freq_list, type = "bigram", position_specific = TRUE) {
  # check length of each list is the same
  n_words <- length(word_list)
  if (n_words != length(freq_list)) {
    stop("Word and Frequency list do not have the same length")
  }

  # apply ngram to each word
  ngram_list <- mapply(word_list, FUN = str_ngrams, rep(type, n_words), frequency = freq_list)

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
    colnames(df2) <- c(type, "pos", "type.frequency", "token.frequency")
  } else {
    df2 <- stats::aggregate(list(df$type_frequency,df$token_frequency),by=list(df$ngram),FUN=sum)
    colnames(df2) <- c(type, "type.frequency", "token.frequency")
  }

  return(df2)
}


#' Caculate ngram frequency of a string
#'
#' @param the_str the character string
#' @param ngram_table table with ngrams
#' @param position_specific TRUE or FALSE
#' @param frequency type or token
#' @param func summed
#'
#' @return summed ngram frequency
#'
str_ngram_frequency <- function(the_str, ngram_table, type = "bigram", position_specific = TRUE, frequency = "token", func = "summed") {
  if (is.null(ngram_table)) {
    stop("ngram_table missing")
  }

  t <- switch(type, "monogram" = 1, "bigram" = 2, "trigram" = 3, 2)

  the_str_len <- stringr::str_length(the_str)
  if (the_str_len > t) {
    last_string_pos <- the_str_len - (t - 1)
  } else {
    last_string_pos <- 1
  }

  f <- 0
  p <- 1
  for (ngram_start in 1:last_string_pos) {
    ngram_end <- ngram_start + (t - 1)
    ngram_str <- stringr::str_sub(the_str, ngram_start, ngram_end)

    switch(frequency,
           "token" = {
              if (position_specific == TRUE) {
                # check if ngram exists
                if (nrow(ngram_table[ ngram_table[[type]] == ngram_str, ]) == 1) {
                  f <- f + ngram_table[ngram_table[[type]] == ngram_str & ngram_table[["pos"]] == p,"token.frequency"]
                }
                  p <- p + 1
              } else {
                  # sum across positions if exists
                  if (nrow(ngram_table[ ngram_table[[type]] == ngram_str, ]) == 1) {
                    f <- f + sum(ngram_table[ngram_table[[type]] == ngram_str,"token.frequency"])
                  }
              }
           },
           "type" = {
              if (position_specific == TRUE) {
                # check if exists in table
                if (nrow(ngram_table[ngram_table[[type]] == ngram_str & ngram_table[["pos"]] == p,]) == 1) {
                  f <- f + ngram_table[ngram_table[[type]] == ngram_str & ngram_table[["pos"]] == p,"type.frequency"]
                }
                p <- p + 1
              } else {
                # sum across positions if found
                if (nrow(ngram_table[ngram_table[[type]] == ngram_str, ]) == 1) {
                  f <- f + sum(ngram_table[ngram_table[[type]] == ngram_str,"type.frequency"])
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

#' ngram info for each word (summed frequency) in a list
#'
#' @param word_list list of words
#' @param ngram_table ngram table
#' @param type monogram, bigram, or trigram
#' @param position_specific TRUE or FALSE
#' @param frequency type or token
#' @param func summed
#'
#' @return vector of frequencies
#'
str_ngram_info <- function(word_list, ngram_table, type = "bigram", position_specific = TRUE, frequency = "token", func = "summed") {
  if (is.null(ngram_table)) {
    stop("ngram_table missing")
  }
  f <- c()
  word_list <- as.character(word_list)

  f <- as.numeric(lapply(word_list,
                         str_ngram_frequency,
                         ngram_table = ngram_table,
                         type = type,
                         position_specific = position_specific,
                         frequency = frequency,
                         func = func))

  return(f)
}

#' Create all possible anagrams of the letter string
#'
#' @param the_str string of letters
#'
#' @return vector of all possible anagrams
#'
anagrams <- function(the_str) {
  letters <- unlist(strsplit(the_str,""))

  the_permutations <- combinat::permn(letters)
  the_list <- unlist(lapply(the_permutations, FUN=paste, sep="",collapse=""))
  the_list <- unique(the_list)
  return(the_list)
}

#' Summed Bigram Frequency (SBF) Rank
#'
#' Based on definition found in Novick & Sherman (2004)
#'
#' @param the_Str the letter string
#' @param bigram_table bigram table
#' @param top12 TRUE or FALSE
#' @param method method to be used
#'
#' @return list with SBF, SBF rank, and anagrams with SBF >= SBF the_str
#'
sbf_rank <- function(the_str, bigram_table, top12 = FALSE, method = "Novick") {
  bigrams <- str_bigrams(the_str, type = "adjacent")
  the_anagrams <- anagrams(the_str)

  switch(method,
         "Novick" = {
           # Method based on Novick & Sherman (2004)

           bf <- c() # list of frequencies of each bigram
           p <- 1 # position
           for (b in bigrams) {
             # frequency bigram
             frequency <- bigram_table[bigram_table$bigram == b & bigram_table$pos == p, "type.frequency"]
             bf <- c(bf, frequency)
             p <- p + 1
           }

           # summed bigram frequency of the_str
           #sbf_the_str <- str_summed_ngram(the_str, bigram_table, type = "bigram", frequency = "type")
           sbf_str_str <- str_ngram_frequency(the_str, bigram_table, type = "bigram", frequency = "type")

           # data.frame with anagrams
           df <- data.frame(anagrams = the_anagrams, sbf = 0)
           df$sbf <- str_ngram_info(df$anagrams, bigram_table, type = "bigram", frequency = "type")

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
         "Gilhooly" = { # --------------- NOT YET WORKING PROPERLY ------------------

           # method based on Gilhooly (1978)
           # requires type frequencies
           sbf_the_str <- 0

           bf <- c() # list of frequencies of each bigram
           p <- 1 # position
           for (b in bigrams) {
             # frequency bigram
             frequency <- bigram_table[bigram_table$bigram == b & bigram_table$pos == p, "type.frequency"]
             # store frequency of correct
             bf <- c(bf, frequency)
             # summed bigram frequency
             sbf_the_str <- sbf_the_str + frequency

             p <- p + 1
           }

           # data.frame with anagrams
           df <- data.frame(anagrams = the_anagrams, sbf = 0)
           df$sbf <-
             str_ngram_info(df$anagrams,
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

#' Count number of bigrams in the_str that exist in the bigram_table.
#' Match on bigram and position
#'
#' Based on definition of Mendelsohn (1976)
#'
#'@param the_str the string
#'@param bigram_table bigram table
#'
#'@return list of bigrams
#'
gtzero <- function(the_str, bigram_table) {
  anagrams <- anagrams(the_str)
  n <- length(anagrams)

  bt <- str_calc_ngrams(anagrams, rep(1,n), type = "bigram", position_specific = T)

  # find the bigrams that are in the bigram_table
  # match on bigram and position
  found <- merge(bt[, c("bigram","pos")], bigram_table, by = c("bigram","pos"))
  gtz <- nrow(found)

  return(gtz)
}


