
###############################################
###############################################
###                                         ###
###       NATURAL LANGUAGE PROCESSING       ###
###                                         ###
###############################################
###############################################

library(tidytext)
library(SnowballC)
library(tm)
library(widyr)
library(topicmodels)
library(broom)
library(h2o)

########################################
##                 Basics             ##
########################################

# Get indexes of matches. Set value = TRUE to get the words instead.
grep("pattern", data, value = FALSE)

# Find and replace pattern. If "pattern" contains a group, i.e. some expression
# wrapped in (), you can back-reference in "replacement";  "\\1" is the first
# group, "\\2" the second one, and so on.
gsub("pattern", "replacement", data)

# Below are common regex expressions. Note that in R, you must always use two backslashes.
\\w    # Alphanumeric characters
\\d    # Digits
\\s    # Space
\\S    # Any non-space characters
.      # One occurrence of anything
{i}    # exactly i ocurrences (put after expression)
{i,j}  # between i and j ocurrences (put after expression)
?      # Zero or one occurences of anything (= {0,1})
+      # One or more occurrences of (= {1,})
*      # Zero or more occurrences of (= {0,})
^      # Starts with (put before expression)
$      # Ends with (put after expression)
[ab]   # "a" or "b" (can also be used for escaping)
(ab)   # "ab" (used to group s.t. special expressions are applied as desired)
[^A]   # Anything but "A"
|      # OR
&      # AND



########################################
##           Pre-Processing           ##
########################################

# Tokenize text. Tokenization means splitting a piece of text into smaller units
# called "tokens" (e.g. words, subwords, or characters). We can do this with
# unnest_tokens(). The function below does this; it also removes punctuation
# marks (- , . ') if it's followed by a space and converts everything to lower
# case. The first argument is a a dataframe with data, "output" defines the
# label of the output column,  "input" is the column in the tibble to use, and
# "token" defines the unit for tokenization (regex" also requires a "pattern"
# argument). The output is a dataframe with a column column with the chosen
# tokenization unit. After you did this (and all other steps outlined below),
# make sure to count and sort the resulting text. If you are tokenizing multiple
# texts (e.g. chapters or tweets), make sure count by the column indicating the
# text and by the chosen token unit.
tokens <- unnest_tokens(data, 
                        output = "word", 
                        input = text_column,
                        token = c("characters", "words", "sentences", "lines", "paragraphs", "regex"))

tokens %>% 
  count(text_col_id, word, sort = TRUE)

# Remove stop words. Stop words are common words we are not actually interested
# in, e.g. "the" or "of". To remove them, anti-join the tokenized text with
# another dataframe containing stop words. The "stop_words" tibble comes with
# tidytext and contains many stop words. Try it and count the resulting tokens
# again; if there is still junk at the top, add these words to the stop_words
# tibble and run again.
tidy_tokens <- tokens %>% 
                  anti_join(stop_words)

tidy_tokens %>% 
  count(text_col_id, word, sort = TRUE)

own_stop_words <- add_row(stop_words, 
                          word = "pesky_word", 
                          lexicon = "custom")

# Stem text. Stemming means transforming words into their common root, e.g.
# "enlisted" and "enlisting" both become "enlist". To do this, use the
# wordStem() function.
stemmed_tokens <- tidy_tokens %>% 
                    mutate(word = wordStem(word))

tidy_tokens %>% 
  count(text_col_id, word, sort = TRUE)


########################################
##                 Corpora            ##
########################################

# A corpus is a just a collection of texts. To see an example of a corpus, run
# the code below to get 50 Reuters articles. The corpus is a list object with an
# element for each article. Each element contains another two elements: the
# content and some metadata.
data("acq")

# To turn a corpus into a tibble, use the tidy() function. 
tidy_data <- tidy(corpus)

# To turn the column of a tibble into a corpus, first run the VCorpus() and
# VectorSource() functions on the column containing the content. This creates
# the list object with the contents; assign it to an object. The metadata must
# then be added manually. Look up the respective elements in the new corpus-object 
# and the tidy tibble, then assign the content from one to the other as shown
# below.
corpus <- VCorpus(VectorSource(tidy_data$content_col))
meta(corpus, "Meta Heading 1") <- tidy_data$meta_heading_1
meta(corpus, "Meta Heading 2") <- tidy_data$meta_heading_2

# Both corpora and tibbles are fine for storing text. However, knowing how to
# convert is useful since corpora are common when working with text data.



########################################
##         Representations            ##
########################################

# Most algorithms prefer orderly, numerical data over messy text. There are
# different ways to represent our text as something we can use to train a
# model.


# BoW representation.  The Bag-of-Words representation reflects how important a
# word is to a text in a corpus of texts. Basically, this is just word-count by
# text. by The R-version of BoW is found by tokenizing, removing stop words, and
# counting words by text ID (e.g. chapters).
unnest_tokens(data, 
              output = "word",
              input = text_column,
              token = "words") %>% 
  anti_join(stop_words) %>% 
  count(text_id, word, sort = TRUE)

# TFIDF representation. The Term Frequency Inverse Document Frequency also reflects
# how important a word is to a text in a corpus of texts. TFIDF considers two
# components. Term Frequency is just the proportion of a given text made up by
# a specific term. The Inverse Document Frequency is the weight of how common
# this term is across all texts, and is computed as log(#_texts_in_corpus /
# #_texts_containing_term). The more common a term is across texts, the closer
# IDF is to zero (a common occurrence of a term in a text is not very special if
# it's generally common in the corpus). The TFIDF value of a given text is then
# TF*IDF. Low values imply that the text mentioned the term not a lot or that
# the term is used by many other texts, so it must be less relevant. because too
# many articles used it. In R, we can get this by also adding bind_td_idf() to
# our pipe. The "n" argument is the word count column produced by count() in the
# previous line. The resulting tibble now also contains "tf", "idf", and "tfidf"
# columns.
unnest_tokens(data, 
              output = "word",
              input = text_column,
              token = "words") %>% 
  anti_join(stop_words) %>% 
  count(text_id_col, word, sort = TRUE) %>% 
  bind_tf_idf(text_id, word, n)


# Cosine similarity. This builds is used to turn BoW or TFIDF into a meaningful
# measure. The idea is to take two texts, to represent each as multidimensional
# vector, and to compute the angle (=cosine) between them (which can be done
# with using the word-count or TFIDF). This is done for all possible two-text
# combinations. This computed angles measure the pairwise similarities, with 0
# meaning no similarity and 1 meaning they're the same. In R, we can get this by
# also adding the pairwise_similarity() function. The first argument is a
# tibble, "item" are the items to compare (articles, tweets, ...), "feature" is
# the column with the feature of interest (words, characters, ...), and "value"
# is the column of comparison values (n for BOW or tif_idf for TFIDF). The
# results likely differ between the two approaches. For classification problems
# with known signal words, use BoW; if the signal words are unknown or you
# just want to check similarity, use TFIDF.
unnest_tokens(data, 
              output = "word",
              input = text_column,
              token = "words") %>% 
  anti_join(stop_words) %>% 
  count(text_id, word, sort = TRUE) %>% 
  bind_tf_idf(word, text_id, n) %>% 
  pairwise_similarity(chapter, word, c(n, tf_idf)) %>%
  arrange(desc(similarity))



########################################
##      Classification Workflow       ##
########################################

# Below is the workflow for classification models. We use text as input to
# predict a binary outcome. The outcome should be a variable already included in
# the data (left-wing tweet=0, right-wing tweet = 1). If it is something
# referring to a term in the texts, see below for a slightly different workflow.

# STEP 1: Clean/Prepare data. We need to prepare input and output variables
# separately. Start with the input. First tokenize, (if required: remove stop
# words), stem words, and count by text id and token unit (sentence, word, ...).
# Next, pipe this into the cast_dtm() function. This turns the results from the
# first step into a document-term-matrix, which contains one row for each
# document (=chapter, tweet, .. determined by "text_id") and one column for each
# word, so the values indicate how often a word appears in a document. The first
# argument is the tibble from the first step, "document" is the text id column
# (e.g. chapters), "term" is the word column, value is the word-count created by
# count() in the previous line, and "weighting" determines how to compute the
# values (keep that as below).
clean_tokens <- unnest_tokens(data, 
                              output = "word",
                              input = text_column,
                              token = "words") %>% 
                  anti_join(stop_words) %>% 
                  mutate(word = wordStem(word)) %>% 
                  count(text_id, word)
  
data_matrix <- cast_dtm(clean_tokens,
                        document = text_id, 
                        term = word,
                        value = n, 
                        weighting = tm::weightTfIdf)
  
# Print the resulting matrix and check the sparsity (=perc of entries being 0).
# If this value and the matrix itself is large, this could become
# computationally intense later on. If you experience performance issues, use
# removeSparseTerms() to remove some columns for words giving many zeroes using a
# pre-defined maximum sparsity threshold; if you set it too low, this could wipe
# out too many terms to train your model.
print(data_matrix)
data_matrix <- removeSparseTerms(data_matrix, sparse = 0.9)

# The data matrix above can be used as the input to our model. The outcome
# variable should come as a binary vector, e.g. 0/1 for whether a token contains
# a word or not. Simply extract it as below from the tokenized tibble.
outcome_vector <- clean_tokens$y

# STEP 2: Create training/test sets for input matrix and outcome vector.
set.seed(42)
sample_size <- floor(0.75 * nrow(data_matrix))
train_index <- sample(1:nrow(data_matrix), size = sample_size)
train_x <- data_matrix[train_index, ]
test_x <- data_matrix[-train_index, ]
train_y <- as.factor(outcome_vector[train_index])
test_y <- as.factor(outcome_vector[-train_index])

# STEP 3: Train model on training data. The example below grows a random forest
# using the randomForest package. The "x" argument is the document-term-matrix
# with the input variables (converted to a normal matrix and then a dataframe),
# "y" is a vector of the binary outcome variable.
model <- randomForest(x = as.data.frame(as.matrix(train_x)), 
                      y = train_y,
                      nTree = 50)

# STEP 4: Check accuracy. Use your model to make predictions on
# the test set, then create a confusion matrix.
pred <- predict(model, as.data.frame(as.matrix(test_x)))
confusionMatrix(pred, test_y)



##################

# The workflow above is used if the binary outcome variable is already included
# in the data. If it relates to the text and doesn't exist yet, there are
# additional preliminary steps. The rest is as shown above. We will try to
# predict if a sentence came from person A or person B.

# STEP 0: Tokenize data by sentence.
tokens <- unnest_tokens(raw_data, 
                        output = "sentence",
                        input = text_column,
                        token = "sentences")

# Use grepl() to add 0/1 variables for whether a sentence includes the name of
# person A or B. 
tokens$person_A <- grepl('person A', tokens$sentence)
tokens$person_B <- grepl('person B', tokens$sentence)

# Use gsub() to remove both persons names from all texts, so that
# the classification algo won't use it when training.
tokens$sentence <- gsub("person A", "person X", tokens$sentence)
tokens$sentence <- gsub("person B", "person X", tokens$sentence)

# Subset to only sentences containing exactly one person's name, since the rest
# of the data is not relevant for training the model.
data <- tokens[tokens$person_A + tokens$person_B == 1]

# Add a sentence_id column.
data$sentence_id <- c(1:nrow(person_sentences))

# STEPS 1-4: Exactly as mentioned before. Yes, this means that we again start by
# tokenizing the newly created "data" object by words.



########################################
##              Topic Modeling        ##
########################################

# Topic modeling tries to identify general "topics" in a corpus of texts. A
# common method to do this is Latent Dirichlet Allocation, which we will use
# here. LDA is based on two basic principles: Each text is made up by a small
# mixture of topics, and certain words in each text can be attributed to a
# certain topics. For example, a sports article might contain 70% news and 30%
# gossip. These topics are a mixture of words, with "trade"/"new"/"move"
# relating to news, and "angry"/change"/"money" relating to gossip.

# STEP 1: Tokenize, remove stop words, stem. Then count and create a
# document-term-matrix, now using TF weights instead of TFIDF weights.
clean_tokens <- unnest_tokens(data, 
                              output = "word",
                              input = text_column,
                              token = "words") %>% 
                  anti_join(stop_words) %>% 
                  mutate(word = wordStem(word)) %>% 
                  count(text_id, word)

data_matrix <- cast_dtm(clean_tokens,
                        document = text_id, 
                        term = word,
                        value = n, 
                        weighting = tm::weightTf)
  

# STEP 2: Find optimal number of topics. The LDA algorithm requires us to
# specify the number of topics to create. To determine it, we can look at
# "perplexity". It measures how well a probability model (like LDA) fits new
# data, with lower values indicating a better fit. Use to code below to first
# create train/test sets. Run the code below to get a plot of perplexity per
# number of topics (no code adjustments needed). Use the number of topics at
# which perplexity doesn't decrease much more.
sample_size <- floor(0.9 * nrow(data_matrix))
set.seed(42)
train_index <- sample(nrow(data_matrix), size = sample_size)
train <- matrix([train_index, ])
test <- matrix([-train_index, ])

for(i in 2:40) {
  lda_model <- LDA(train, k = i, method = "Gibbs", control = list(seed = 42))
  values <- v(values, perplexity(lda_model, test))
}

plot(c(2:40), values, main = "Perplexity for Topics", xlab = "Number of Topics", ylab = "Perplexity")


# STEP 3: Train model. The first argument is the data, "k" is the number of
# topics, "method" defines the underlying algo to use (keep as below), and
# control can be used to set a seed (keep as well).
data_lda <- LDA(train, k = 4, method = "Gibbs", control = list(seed = 42))

  
# STEP 4: Extract beta values and check results. The code below returns a tibble
# with three columns: topic, word and beta. The latter is a word distribution
# per topic, with higher values indicating that a word is more closely related
# to a topic than other words. The sum of all beta values for a given topic is
# 1, so the whole beta column sums to the number of topics.
data_betas <- tidy(data_lda, matrix = "beta")

# Adjust the last line to get most closely related words for a topic. If a
# certain word has a high beta value in many topics, consider adding it to the
# stop_words list and re-run the model (might not be too relevant).
data_betas %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, desc(beta)) %>% 
  filter(topic == c(1,2,...k))
  
  
# STEP 5: Extract gamma values and compare to beta values. The code below
# returns a tibble with three columns: document, topic, and gamma. The latter is
# like a topic distribution per text (e.g. chapter). The values for each text
# should sum to 1.
data_gammas <- tidy(data_lda, matrix = "gamma")

# Adjust the last line to get an overview of gamma values for each chapter.
data_gammas %>% 
  filter(document == c("Chapter 1", "Chapter 2", "..."))


# STEP 6: Compare beta and gamma by topic and make sense of results. Have a look
# at the beta and gamma values for the same topics, both at the upper and the
# lower value range. Check if the results are reasonable and if you can see a
# narrative. If not, consider adjusting the number of topics and running the
# model again. LDA is often more about practical use than about getting a low
# perplexity. Two rules of thumb: Use a small number of topics where each topic
# is represented by several texts; only use large topic counts if you have time
# to explore and dissect each topic.
data_betas %>%
  filter(topic == 2) %>%
  arrange(beta)
data_gammas %>%
  filter(topic == 3) %>%
  arrange(beta)



# STEP 7: Use results. Extract top words and texts for each topic with the code
# below. Read the actual texts to find "themes" for each topic. Alternatively,
# let someone with more knowledge do it.
data_betas %>% 
  filter(topic == c(1,2,...)) %>% 
  arrange(desc(beta)) %>% 
  select(word)

data_gammas %>% 
  filter(topic == c(1,2,...)) %>% 
  arrange(desc(gamma)) %>% 
  select(document)


# REMARKS: The code below may be useful.

  # How many times each topic was highest weighted topic?
  data_gammas %>% 
    group_by(document) %>% 
    arrange(desc(gamma)) %>% 
    slice(1) %>% 
    group_by(topic) %>% 
    tally(topic, sort = TRUE)
  
  # How strong was topic when it was on top?
  data_gammas %>% 
    group_by(document) %>% 
    arrange(desc(gamma)) %>% 
    slice(1) %>% 
    group_by(topic) %>% 
    summarize(avg = mean(gamma)) %>% 
    arrange(desc(avg))

  

########################################
##          Sentiment Analysis        ##
########################################

# SA is used to asses the subjective information in a text, e.g. whether it's
# positive/negative or if the words convey some form of emotion. The workflow
# below shows how this works. There are two ways to do this: Manually or with
# h2o. Both are shown below.
  
  
# STEP 1: Prepare data. Tokenize and remove stop words (and nothing else).
data_tokens <- unnest_tokens(data, 
                             output = "word",
                             input = text_column,
                             token = "sentences") %>% 
                anti_join(stop_words)
  
  
# STEP 2: Add sentiments. SA requires a dictionary relating words to labels or
# numerical scores. Use tidytext's built-in sentiments data
# ("positive"/"negative") or get_sentiments() with access to AFINN
# (-5="extremely negative" to  5="extremely positive)", bing
# ("positive"/"negative"), or nrc (multiple labels). Inner join one of these
# dictionaries with the tokenized tibble.
dict <- tidytext::sentiments
dict <- get_sentiments(lexicon = c("afinn", "bing", "nrc"))
  
data_sentiment <- data_tokens %>% 
                    inner_join(dict)

# STEP 3: Subset the data to whatever you're interested in. For example, this
# loop filters to sentences containing certain terms, tokenizes them and
# removes stop words, and uses the afinn lexicon to compute the overall
# sentiment score. This is done for all three terms.
for(term in c("term_1", "term_2", "term_3")) {
  
  term_sentences <- sentences[grepl(term, sentences$sentence), ]

  term_tokens <- term_sentences %>%
                  unnest_tokens(output = "word", 
                                token = "words", 
                                input = sentence) %>%
                  anti_join(stop_words)

  result <- term_tokens %>% 
    inner_join(get_sentiments("afinn")) %>%
    summarise(sentiment = sum(score))

  print(paste0(term, ": ", result$sentiment))
}


# This code counts tokenized text and removes stop words, subsets it to words
# that relate to certain emotions using the nrc dictionary, and finds to top
# words for these emotions.
data_tokens <-unnest_tokens(data,
                            output = "word", 
                            input = content) %>%
                anti_join(stop_words)

anticipation <- get_sentiments("nrc") %>% 
                  filter(sentiment == "anticipation")
joy <- get_sentiments("nrc") %>% 
          filter(sentiment == "joy")

data_tokens %>%
  inner_join(anticipation, by = "word") %>%
  count(word, sort = TRUE)
data_tokens %>%
  inner_join(joy, by = "word") %>%
  count(word, sort = TRUE)
  
  
######################

# STEP 1: Initialize h2o session.
h2o.init()

# STEP 2: Convert data to h2o object, tokenize words from column containing
# texts (h2o will put an NA at the end of each token), convert to lower case,
# and remove stop words.
h2o_object <- as.h2o(data)
data_tokens <- h2o.tokenize(h2o_object$text_col, "\\\\W+")
data_tokens <- h2o.tolower(data_tokens)
data_tokens <- tweet_words[is.na(data_tokens) || (!data_tokens %in% stop_words$word),]

# STEP 3: Train model vector model. The algo creates a vector space where
# similar words are closer to each other. This approach avoids issues like words
# have another meaning after removing stop words. The "min_word_freq" removes
# all words appearing less than the specified number of times, "epochs" is the
# number of training iterations (for larger texts, use higher values).
model <- h2o.word2vec(words, 
                      min_word_freq = 5, 
                      epochs = 10)
 
# STEP 4: Find similar terms. The model has several applications, but one is to
# find similar terms. Input the model and a term for which related terms in the
# text should be found.
h2o.findSynonyms(model, "term")

