# load libraries
lib <- c("magrittr", "tidyverse")
lapply(lib, require, character.only = TRUE)
rm(lib)


# load homemade funs
source("homemade_funs.R")

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

# save mot size unfiltered for plurals and root verbs
# save mot tokens
mot_tokens <- length(unlist(mot_na$string))
mot_ort_types <- nrow(mot_phon)
mot_ph_types <- nrow(mot_uni_len)

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

# orthographic types, most frequent gram_cat assigned to each word
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
mot_ort_types_filtered <- nrow(mot_mor_uni)

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
# grammatical categories types
table_add <- function(table, name1, name2, name3 = NULL) {
  # add and delete values from a table
  if (length(name3) == 0) {
    table[name1] <- table[name1] + table[name2]
    table <- table[names(table) != name2]
  } else {
    table[name1] <- table[name1] + table[name2]
    table[name3] <- table[name3] + table[name2]
    table <- table[names(table) != name2]
  }
  table
}

mot_uni_mor_raw <- mot_mor_uni$cat %>%
  table() %>%
  sort(decreasing = T) %>%
  table_add("N", "N:PROP") %>%
  table_add("N", "N|-N-CL") %>%
  table_add("N", "N-CL") %>%
  table_add("N", "N:LET") %>%
  table_add("N", "N:PROP|-N-CL") %>%
  table_add("PRO", "PRO:INDEF") %>%
  table_add("V", "V:AUX") %>%
  table_add("N", "N:PROP|-N-CL|V", "V") %>%
  table_add("N", "N|-N-CL|V", "V") %>%
  table_add("ADV", "WH:ADV") %>%
  table_add("PRO", "PRO:DEM") %>%
  table_add("PRO", "PRO:REFL") %>%
  table_add("PRO", "PRO:DEM|-N-CL|V", "V") %>%
  table_add("PRO", "PRO:POSS") %>%
  table_add("PRO", "PRO|-N-CL") %>%
  table_add("V", "V|-V-CL|NEG") %>%
  table_add("PRO", "WH:PRO") %>%
  table_add("ADV", "ADV:INT") %>%
  table_add("ADV", "ADV|-N-CL|V", "V") %>%
  table_add("N", "N|-PL-N-CL") %>%
  table_add("V", "INF") %>%
  table_add("PRO", "PRO:INDEF|-N-CL|V", "V") %>%
  table_add("PRO", "PRO|-N-CL|V", "V") %>%
  table_add("V", "V|-N-CL|V") %>%
  table_add("PRO", "WH:PRO|-N-CL|V", "V") %>%
  table_add("ADV", "ADV|-N-CL") %>%
  table_add("N", "N:PREP|-N-CL") %>%
  table_add("N", "N:PROP|-PL-N-CL") %>%
  table_add("N", "N|-DIM-N-CL|V", "V") %>%
  table_add("N", "N|-V-CL", "V") %>%
  table_add("PRO", "PRO:POSS|-N-CL|V", "V") %>%
  table_add("V", "V|-CL|PRO", "PRO") %>%
  table_add("V", "V|-PROG~INF") %>%
  table_add("ADV", "WH:ADV|-N-CL|V", "V") %>%
  (function(x) {
    perc <- prop.table(x)*100
    
    cats <- c("N", "V", "ADJ", "ADV", "PRO")
    
    x[names(x) %in% cats] %>%
      sort(decreasing = TRUE) %>%
      (function(x) {
        cats <- names(x)
        
        x <- tibble(cat = names(x), freq = x, perc = perc[cats])
        x[x == "N"] <- "NOUN"
        x[x == "V"] <- "VERB"
        x[x == "PRO"] <- "PRON"
        x
      })
  }) %>%
  round_df(2)

# grammatical categories tokens (last word utterance)
mot_mor_raw_last <- mot_mor_filter_last$mor %>%
  table() %>%
  sort(decreasing = T) %>%
  (function(x) x[x>10]) %>%
  table_add("PRO", "PRO:DEM") %>%
  table_add("N", "N:PROP") %>%
  table_add("N", "N-PL") %>%
  table_add("PRO", "PRO:INDEF") %>%
  table_add("V", "V-PROG") %>%
  table_add("PRO", "WH:PRO") %>%
  table_add("V", "V&PERF") %>%
  table_add("V", "V&PRES") %>%
  table_add("V", "V&3S") %>%
  table_add("V", "V-PAST") %>%
  table_add("V", "V:AUX") %>%
  table_add("V", "V:AUX-V-CL|NEG|NOT") %>%
  table_add("V", "V-3S") %>%
  table_add("ADV", "ADV-LY") %>%
  table_add("ADV", "WH:ADV") %>%
  table_add("V", "V&PAST") %>%
  table_add("N", "N-DIM") %>%
  table_add("PRO", "PRO:EXIST") %>%
  table_add("ADJ", "ADJ&CP") %>%
  table_add("N", "N&PL") %>%
  table_add("N", "N:LET") %>%
  table_add("ADJ", "ADJ-CP") %>%
  table_add("V", "V:AUX&PAST") %>%
  table_add("PRO", "PRO:REFL") %>%
  table_add("V", "V-PERF") %>%
  table_add("V", "V:AUX&3S") %>%
  table_add("PRO", "PRO:INDEF-PL") %>%
  table_add("V", "INF") %>%
  table_add("PRO", "PRO:POSS") %>%
  table_add("N", "N:PROP-N-CL|POSS") %>%
  table_add("N", "N:PROP--N-CL|POSS") %>%
  table_add("V", "V&PAST&13S") %>%
  table_add("V", "V&3S-V-CL|NEG|NOT") %>%
  table_add("PRO", "PRO-N-CL|POSS") %>%
  table_add("V", "V:AUX&PAST-V-CL|NEG|NOT") %>%
  table_add("N", "N-DIM-PL") %>%
  table_add("V", "V:AUX&3S-V-CL|NEG|NOT") %>%
  table_add("PRO", "PRO:DEM-N-CL|V|BE&3S", "V") %>%
  table_add("PRO", "PRO-N-CL|V|BE&3S", "V") %>%
  table_add("ADJ", "ADJ-SP") %>%
  table_add("N", "N:PROP-PL") %>%
  table_add("V", "V:AUX&PRES") %>%
  table_add("V", "V&1S") %>%
  table_add("ADJ", "ADJ&SP") %>%
  table_add("ADV", "ADV:INT") %>%
  table_add("N", "N--N-CL|POSS") %>%
  table_add("N", "N-N-CL|POSS") %>%
  table_add("PRO", "WH:PRO-N-CL|V|BE&3S", "V") %>%
  table_add("N", "N:PROP-POSS") %>%
  table_add("PRO", "PRO-N-CL|V|BE&PRES", "V") %>%
  table_add("V", "V-V-CL|NEG|NOT") %>%
  table_add("ADJ", "ADJ-PL") %>%
  table_add("ADJ", "ADJ-LY") %>%
  table_add("PRO", "PRO-PL") %>%
  table_add("ADV", "ADV-PL") %>%
  table_add("V", "V:AUX~NEG|NOT") %>%
  table_add("PRO", "PRO-N-CL|V:AUX|WILL", "V") %>%
  table_add("N", "N-N-CL|V|BE&3S", "V") %>%
  table_add("V", "V:AUX&PAST&13S") %>%
  table_add("ADV", "WH:ADV-N-CL|V|BE&3S", "V") %>%
  table_add("V", "V-3S-V-CL|NEG|NOT") %>%
  table_add("V", "V&PAST&13S-V-CL|NEG|NOT") %>%
  table_add("N", "N:PROP-N-CL|V|BE&3S", "V") %>%
  table_add("PRO", "PRO:INDEF-N-CL|V|BE&3S", "V") %>%
  table_add("V", "V:AUX&1S") %>%
  table_add("V", "V&PAST-V-CL|NEG|NOT") %>%
  (function(x) {
    perc <- prop.table(x)*100
    
    cats <- c("N", "V", "ADJ", "ADV", "PRO")
    
    x[names(x) %in% cats] %>%
      sort(decreasing = TRUE) %>%
      (function(x) {
        cats <- names(x)
        
        x <- tibble(cat = names(x), freq = x, perc = perc[cats])
        x[x == "N"] <- "NOUN"
        x[x == "V"] <- "VERB"
        x[x == "PRO"] <- "PRON"
        x
      })
  }) %>%
  round_df(2)

#### Spoken BNC ####
# import spok_bnc texts
spok_bnc_paths <- read_tsv("spoken_bnc_path.txt", col_names = F) %>%
  mutate(name = `X1`,
         path = str_replace(`X1`, "$", ".txt") %>%
           str_replace("^", paste("/Users/francesco/Documents/BNC/texts\ txt", "/", sep = ""))) %>%
  select(name, path)

spok_bnc_txt_raw <- spok_bnc_paths$path %>%
  sapply(function(x) {
    read.delim(x, header = F, stringsAsFactors = F)
  })

spok_bnc_txt <- spok_bnc_txt_raw %>%
  sapply(function(x) {
    str_extract_all(x, "c5=[^ ]* hw=[a-zA-Z]* pos=[A-Z]+>[^<]*<")
  }) %>%
  (function(x) {
    names(x) <- spok_bnc_paths$name
    x
  })
