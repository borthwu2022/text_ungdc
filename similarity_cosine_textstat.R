library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda)
library(tidyverse)
library(countrycode)
#Creating corpus for each year

simil.list <- list()
for (i in 1:79) {
  print(i)
ungdc.i <- corpus_subset(ungd_corpus, Year== i+1945)

tok <- tokens(ungdc.i, what = "word",
              remove_punct = TRUE,
              remove_symbols = TRUE,
              remove_numbers = TRUE,
              remove_twitter = TRUE,
              remove_url = TRUE,
              remove_hyphens = TRUE,
              verbose = TRUE)

# stemming re-introduced 
dfm <- dfm(tok, 
           tolower = TRUE,
           remove=stopwords("english"),
           stem=TRUE, 
           verbose = TRUE)

#Removing any digits. `dfm` picks up any separated digits, not digits that are part of tokens.
#Removing any punctuation. `dfm` picks up any punctuation unless it's part of a token.
#Removing any tokens less than four characters.
dfm.m <- dfm_select(dfm, c("[\\d-]", "[[:punct:]]", "^.{1,3}$"), selection = "remove", 
                    valuetype="regex", verbose = TRUE)


#Dropping words that appear less than 5 times and in less than 3 documents.
dfm.trim <- dfm_trim(dfm.m, min_termfreq = 5, min_docfreq = 3)

#tfidf weighting
dfm.w <- dfm_tfidf(dfm)

#holders for country names in distance measures below
#pres <- paste0(presidency$Country_alt[presidency$Year==i])

#Cosine similarity calculations

#doc <- paste0(dfm.w@Dimnames$docs[dfm.w@docvars$Country==pres])

similarities.i <- as.matrix(
  textstat_simil(dfm.w,margin = "documents", method = "cosine"), sorted = FALSE)

similarities.i <- as.data.frame(similarities.i)

colnames(similarities.i) <- quanteda::docvars(ungdc.i)[["Country"]]
  rownames(similarities.i) <- quanteda::docvars(ungdc.i)[["Country"]]
  
simil.list[[i]] <- similarities.i
names(simil.list)[i] <- i+1945
}

filename <- "textstat_cosine_list.RData"
filepath <- file.path("C:/Users/Bort Wu/Desktop/summer_2025/projects/foreign_policy/outputs/word_vectors/", filename)
save(simil.list, file = filepath)

# Plot similarity scores
simil_plot(simil.list, "textstat_cosine")
