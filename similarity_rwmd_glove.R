
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


# # --- Hyperparameters:
# # The standard dimension sizes in the CS literature are chosen from the 
# # following sets: namely word_vectors_size = c(50, 100, 200)
# # and use x_max = c(15, 25).
# # Here we follow the practice from Chelotti et al. 2022
hyper_p <- expand.grid(wvs = 200, x_max = 15)

# --- Use embeddings to find dyadwise speech distances within each year 
for(j in 1:nrow(hyper_p)){
   hyper_p_j <- hyper_p[j,]
   
  print(j)

rwmd_list <- list() # Empty list for similarity scores

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
  
  # Create a term co-occurrence matrix
  ir_tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
  
  # Fit the GloVe model onto the subset data
  glove_model = GloVe$new(rank = hyper_p_j$wvs, x_max = hyper_p_j$x_max)
  
  # Create embeddings from the fitted GloVe model
  wv = glove_model$fit_transform(ir_tcm, n_iter = 15, convergence_tol = 0.001)
  
  # Add the contextual factor to the embeddings
  wv = wv + t(glove_model$components)
  
  # Apply the Relaxed Word Mover's Distance (RWMD)
  rwmd_model <- RWMD$new(x = ir_dtm, embeddings = wv)

  rwmd_sims <-  rwmd_model$sim2(ir_dtm) # calculate similarity 
  
  # Normalize similarity scores
  rwmd_sims <- (rwmd_sims-min(rwmd_sims))/(max(rwmd_sims)-min(rwmd_sims))
  
  # Rename columns and rows as corresponding countries
  
  colnames(rwmd_sims) <- quanteda::docvars(sub.i)[["Country"]]
  rownames(rwmd_sims) <- quanteda::docvars(sub.i)[["Country"]]

  rwmd_list[[i]] <- rwmd_sims
  names(rwmd_list)[i] <- i+1945
  
  }

filename <- "rwmd_list.RData"
filepath <- file.path("C:/Users/Bort Wu/Desktop/summer_2025/projects/foreign_policy/outputs/word_vectors/", filename)
save(rwmd_list, file = filepath)

}






