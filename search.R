
library(litsearchr)

# For PubMed, use "Send to > Citation Manager"
# Download .nbib file

naiveimport <- 
  import_results(file = "search_files/pubmed-person-cen-set.nbib")

naiveresults <- remove_duplicates(naiveimport, field = "title", method = "string_osa")

rakedkeywords <-
  extract_terms(
    text = paste(naiveresults$title, naiveresults$abstract),
    method = "fakerake",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )

taggedkeywords <-
  litsearchr::extract_terms(
    keywords = naiveresults$keywords,
    method = "tagged",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )

all_keywords <- unique(append(taggedkeywords, rakedkeywords))

naivedfm <-
  litsearchr::create_dfm(
    elements = paste(naiveresults$title, naiveresults$abstract),
    features = all_keywords
  )

naivegraph <-
  litsearchr::create_network(
    search_dfm = as.matrix(naivedfm),
    min_studies = 2,
    min_occ = 2
  )

visNetwork::visIgraph(naivegraph)
