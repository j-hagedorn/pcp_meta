
library(text2vec)

stopword_list <- 
  c(
    stopwords::stopwords("en"),
    "medical","health","care","research","study","paper","data","studies","article","analysis",
    "trial","trials","randomized","interview","interviews","control","questionnaire",
    "literature","review","studies","article","pubmed","medline","embase","authors",
    letters
  )

clean_text <- function(x){
  
  x %>%
    str_to_lower() %>%
    str_replace_all("\\[|\\]"," ") %>% # remove square brackets
    str_replace_all( "[^\\s]*[0-9][^\\s]*"," ") %>% # remove mixed string & number
    str_replace_all("\\-"," ") %>% # replace dashes, shd be picked up in noun phrases
    str_squish() %>%
    replace_number(remove = T) %>% # remove number
    replace_html(replacement = "") 
  
}

clean_df <- clean_df %>% mutate(txt_clean = clean_text(txt)) 

iter <- 
  clean_df$txt_clean %>%
  word_tokenizer() %>%
  itoken(ids = clean_df$document)

vocab <- 
  iter %>%
  create_vocabulary(stopwords = stopword_list) %>%
  prune_vocabulary(
    term_count_min = 10, 
    doc_proportion_min = 0.01
  )

get_colloc <- function(iter_obj,vocabulary,stopword_list=c(letters),n_iter=3,colloc_min=50,voc_min_n=10,voc_min_prop=0.01){
  
  colloc_model <- 
    Collocations$new(
      vocabulary = vocab, 
      collocation_count_min = colloc_min
    )
  
  colloc_model$fit(iter_obj, n_iter = n_iter)
  
  # Transform original iterator and vocab to insert collocations
  iter_phrases <- colloc_model$transform(iter_obj)
  
  vocab_phrases <- 
    iter_phrases %>%
    create_vocabulary(stopwords = stopword_list) %>%
    prune_vocabulary(
      term_count_min = voc_min_n, 
      doc_proportion_min = voc_min_prop
    )
  
  output <- list()
  output$model <- colloc_model
  output$iter  <- iter_phrases
  output$vocab <- vocab_phrases
  
  return(output)
  
}

colloc <- iter %>% get_colloc(vocab)


dtm <-
  colloc$iter %>%
  create_dtm(vocab_vectorizer(colloc$vocab))

get_lda <- function(dtm, k = 15){
  
  lda <- LDA$new(k)
  fitted_lda <- lda$fit_transform(dtm)
  
  per_term_n <-
    lda$components %>% 
    t() %>%
    as_tibble() %>%
    mutate(term = colnames(lda$components))  %>%
    rename_all(list(~str_replace_all(.,"^V","topic_"))) %>%
    select(term,everything()) %>%
    pivot_longer(-term) %>%
    rename(count = value)

  per_term_top <- 
    lda$get_top_words(n = 50,lambda = 0.3) %>%
    as_tibble() %>%
    rename_all(list(~str_replace_all(.,"^V","topic_"))) %>%
    pivot_longer(everything()) %>%
    rename(term = value) %>%
    group_by(name) %>%
    mutate(rank = row_number())
  
  per_term <-
    lda$topic_word_distribution %>% 
    t() %>%
    as_tibble() %>%
    mutate(term = colnames(lda$topic_word_distribution))  %>%
    rename_all(list(~str_replace_all(.,"^V","topic_"))) %>%
    select(term,everything()) %>%
    pivot_longer(-term) %>%
    rename(dist = value) %>%
    left_join(per_term_n, by = c("term","name")) %>%
    left_join(per_term_top, by = c("term","name")) %>%
    rename(topic = name)
  
  rm(per_term_n); rm(per_term_top)
  
  per_doc <- 
    fitted_lda %>%
    as_tibble() %>%
    mutate(doc = row.names(fitted_lda)) %>%
    rename_all(list(~str_replace_all(.,"^V","topic_"))) %>%
    select(doc,everything()) %>%
    pivot_longer(-doc) %>%
    group_by(doc) %>%
    mutate(best_topic = paste0("topic_",which.max(value))) %>%
    pivot_wider() %>%
    # Add concatenated top-terms as best_topic description
    left_join(
      per_term %>%
        filter(rank <= 10) %>%
        group_by(topic) %>%
        arrange(rank) %>%
        summarize(topic_desc = paste(term,collapse = "-")),
      by = c("best_topic" = "topic")
    ) %>%
    left_join(
      clean_df %>% select(document,txt),
      by = c("doc" = "document")
    ) %>%
    select(doc,txt,best_topic,topic_desc,everything())
  
  output <- list()
  
  output$model <- lda
  output$per_term <- per_term
  output$per_doc <- per_doc
  
  return(output)
  
}

topics <- dtm %>% get_lda()


