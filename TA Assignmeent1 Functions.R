clean_text = function(text_df){
  
  text_df$bd.text = gsub("<.*?>", " ", text_df$bd.text)
  str(text_df)
  text_df$bd.text  =  iconv(text_df$bd.text, "latin1", "ASCII", sub="") # Keep only ASCII characters
  head(text_df$bd.text,5)
  text_df$bd.text  =  gsub("[^[:alnum:]]", " ", text_df$bd.text)        # keep only alpha numeric 
  text_df$bd.text  =  tolower(text_df$bd.text)   
  text_df$bd.text  =  stripWhitespace(text_df$bd.text)
  text_df$bd.text = gsub("[^A-Za-z\\s]"," ",text_df$bd.text) 
  
  return(text_df)  
  
}

# Function to find out Unigrams
unigram_df = function(text_df){
  
  unigram <-    text_df %>%   
    mutate(doc = file) %>%
    unnest_tokens(word, bd.text) %>% 
    anti_join(stop_words) %>%
    group_by(doc) %>%
    count(word, sort=TRUE)
  
  return(unigram)
}

# Function to find out biigrams and split them to remove stop words and unite them to make unigram
bigram_unigram = function(text_df){
  
  bigram_df = text_df %>%  
    unnest_tokens(bigram, bd.text, token = "ngrams",n=2)  %>%
    group_by(file) %>%
    count(bigram, sort=TRUE)
  
  head(bigram_df,10)            
  str(bigram_df)
  bigrams_segregated <- bigram_df %>%
    separate(bigram, c("word1", "word2"), sep = " ") 
  
  bigram_counts <- bigrams_segregated %>% 
    count(word1, word2, sort = TRUE)
  
  bigrams_filtered <- bigrams_segregated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
  bigram_counts <- bigrams_filtered %>% 
    count(word1, word2, sort = TRUE)
  
  bigrams_united <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = "_")
  
  head(bigrams_united,10)
  
  freq_bigrams = bigrams_united %>% head(10) %>% rename( Count = n,doc =file) 
  head(freq_bigrams,10)
  
  return (freq_bigrams)
  
} 

#Function to annotate text data using ud_model
eng_model_udp <-  function(text){
  
  
  #Annotate text dataset using ud_model above
  
  x <- udpipe_annotate(english_model, x = text) #%>% as.data.frame() %>% head()
  x <- as.data.frame(x) 
  return (x)
}

#Function to find out most common Nouns

common_nouns = function(text_annotate){
  
  # The most common nouns? 
  all_nouns = text_annotate %>% subset(., upos %in% "NOUN") 
  top_nouns = txt_freq(all_nouns$lemma)  # txt_freq() calcs noun freqs in desc order
  return (top_nouns)
  
}

#Function to find out most common Verbs

common_verbs = function(text_annotate){
  
  
  #The most common verbs?
  all_verbs = text_annotate %>% subset(., upos %in% "VERB") 
  top_verbs = txt_freq(all_verbs$lemma)
  return (top_verbs)   
  
}

#Build DTM
create_dtm = function (token_set_merge,doc_list){
  
  dtm_tokens = matrix(0, nrow = 1, ncol = length(token_set_merge))
  row.names(dtm_tokens) = 1
  colnames(dtm_tokens) = token_set_merge
  for (i1 in 1:length(token_set_merge)){    # looping over tokens
    
    for (i2 in 1){
      
      dtm_tokens[i2, i1] = length(grep(token_set_merge[i1], doc_list[[i2]]))
      
    }} # i1 and i2 loops end
  
  return(dtm_tokens)
}
