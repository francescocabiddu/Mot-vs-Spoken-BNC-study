# load libraries
lib <- c("magrittr", "tidyverse")
lapply(lib, require, character.only = TRUE)
rm(lib)


#### prepare mot dfs ####
# load mot raw and mot_uni
load("mot_dfs.RData")

# save types phonetic transcription
mot_phon <- mot_uni %>%
  (function(x) {
    tibble(word = names(unlist(x$phon)), phon = unlist(x$phon)) %>%
      (function(y) {
        y[!duplicated(y$word), ]
      })
  }) %>%
  arrange(word)

# mot mor
mot_mor_filter <- mot_na %>%
  (function(x) {
    tibble(word = unlist(x$string), mor_raw = unlist(x$mor))
  }) %>%
  na.omit() %>%
  mutate(pl = grepl("^N\\|.*PL$", mor_raw),
         verb = grepl("[^A-Z]V[^A-Z]|^V[^A-Z]", mor_raw),
         root = mor_raw %>%
           str_match_all("\\|[A-Z|:]*") %>%
           sapply(as.vector) %>%
           sapply(str_match_all, "[A-Z]+$") %>%
           sapply(as.vector),
         mor = mot_mor$mor) %>%
  arrange(word)
 
# convert plurals, convert to root verbs 
mot_mor_uni <- tibble(word = mot_mor_filter %>%
                        filter(pl == FALSE, verb == FALSE) %>%
                        (function(x) {
                          x$word}) %>%
                        c(mot_mor_filter %>%
                            filter(verb == TRUE | pl == TRUE) %>%
                            (function(x) {
                              unlist(x$root)
                            })))

# save mot tokens
mot_tokens <- nrow(mot_mor_uni) # increased because x@x considered separately as in the BNC

# orthographic types, more frequent gram_cat assigned to each word
mot_mor_uni %<>%
  (function(x) {
    tibble(word = x$word %>%
             unique() %>%
             sort())
  }) %>%
  mutate(cat = sapply(word, function(x) {
    mot_mor_filter %>%
      filter(word == x) %>%
      (function(y) {
        (y$mor %>%
           table() %>%
           sort(decreasing = T))[1] %>%
          names()
      })
      
  }) %>%
    as.character() %>%
    (function(x) {
      x[x == "NULL"] <- NA
      x
    })) %>%
  (function(x) {
    x <- x[!grepl(".+V:AUX|V:AUX.+", x$cat),] # delete wrong V:AUX cases
    x
  })

# save mot types
mot_types <- nrow(mot_mor_uni)

# mot_mor_pp_pn with only plurals converted (orthographic types)
mot_uni_pp_pn <- mot_mor_filter %>%
  (function(x) {
    x$word[which(x$pl == TRUE)] <- x$root[which(x$pl == TRUE)]
    x$word <- unlist(x$word)
    x
  }) %>%
  (function(x) {
    x[!duplicated(x$word), ] %>%
    select(word)
  }) %>%
  mutate(phon = word %in% mot_phon$word) %>%
  filter(phon == TRUE) %>%
  mutate(phon = mot_phon$phon[which(mot_phon$word %in% word)])

# mot_mor last word of utterance
mot_mor_filter_last <- mot_na %>%
  mutate(string = sapply(string, function(x) {
    x[length(x)]
    }),
    mor = sapply(mor, function(x) {
      x[length(x)]
    })) %>%
  select(word = string, mor_raw = mor) %>%
  mutate(pl = grepl("^N\\|.*PL$", mor_raw),
         verb = grepl("[^A-Z]V[^A-Z]|^V[^A-Z]", mor_raw),
         mor = mor_raw %>%
           str_replace("\\|[A-Z|:]*", ""))

#### mot measures ####
# summarize gram cat

# assign iphod pp and pn

#### Spoken BNC ####
# import spokbnc ort keeping root_verb tag  

# match spokbnc ort with phonemic file