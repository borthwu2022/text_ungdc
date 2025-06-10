 # --- Libraries --- #
library(quanteda)  
library(readtext)
library(stringr)
library(stringi)
library(text2vec)
library(wordVectors)
library(readr)
library(countrycode)
library(Rtsne)
library(tidyverse)
library(ggrepel)
library(xtable)
library(cccd)
set.seed(1912)

setwd("C:/Users/Bort Wu/Desktop/summer_2025/projects/foreign_policy/data/ungdc")



# # #--- UNGD Speeches --- #
# Set directory to location of downloaded UNGA corpus
DATA_DIR <- "C:/Users/Bort Wu/Desktop/summer_2025/projects/foreign_policy/data/ungdc"  
 
ungd_files <- readtext(DATA_DIR, 
                       docvarsfrom = "filenames", 
                       dvsep="_", 
               # import, setting document variables in the process
                       docvarnames = c("Country", "Session", "Year"))
 
# Construct a corpus object 
ungd_corpus <- corpus(ungd_files, text_field = "text") 

 
# # #--- Fit GloVe Model --- #

# # Create a word tokenizer function
tok <- function(x) {
  word_tokenizer(x) %>% 
    lapply( function(x) SnowballC::wordStem(x, language="en"))
  } 


cosine_list <- list() # Empty list for similarity scores

for(i in 1:length(1946:2024)){
  
  print(i+1945)
  
  # Subset out the textual data for a given year i+1945
  sub.i <- corpus_subset(ungd_corpus, Year == i+1945)
  
  # Pre-process texts for data analysis
  tokens <- tok(tolower(sub.i))
  it <- itoken(tokens, progressbar = F)
  v <- create_vocabulary(it)
  v = prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.5)
  vectorizer <- vocab_vectorizer(v)
  
  # Create a document-term matrix
  ir_dtm <- create_dtm(it, vectorizer)
  
 # Compute pairwise cosine similarities for ALL rows in ir_dtm
  cosine_sim_mat <- sim2(
  x      = ir_dtm,        # a dgCMatrix, rows = documents
  method = "cosine",
  norm   = "l2"           # L2‐normalize each row before computing dot‐product
  )
  
cosine_norm <- (cosine_sim_mat-min(cosine_sim_mat))/(max(cosine_sim_mat)
                                                     -min(cosine_sim_mat))

   
    colnames(cosine_norm) <- quanteda::docvars(sub.i)[["Country"]]
    rownames(cosine_norm) <- quanteda::docvars(sub.i)[["Country"]]
  
  # Rename columns and rows as corresponding countries
  

  cosine_list[[i]] <- cosine_norm
  names(cosine_list)[i] <- i+1945
  }

filename <- "cosine_list.RData"
filepath <- file.path("C:/Users/Bort Wu/Desktop/summer_2025/projects/foreign_policy/outputs/word_vectors/", filename)
save(cosine_list, file = filepath)

# Plot similarity scores
x <- cosine_list
y <- "cosine"

simil_plot(cosine_list, y)
   