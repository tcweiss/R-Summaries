
###############################################
###############################################
###                                         ###
###              TOPIC MODELING             ###
###                                         ###
###############################################
###############################################

library(tidyverse)
library(topicmodels)
library(tidytext)
library(wordcloud)


########################################
##           Topic Discovery          ##
########################################

# STEP 1: Tokenize. If required, remove stop words (or keep non-stop words with
# inner_join). Then count by document and token unit, convert to dtm.
dtm <- unnest_tokens(data, 
                     output = output_col, 
                     input = text_col,
                     token = "words") %>% 
       anti_join(stop_words) %>% 
       count(chapter, word) %>% 
       cast_dtm(document = text_id, 
                term = output_col, 
                value = n)


# STEP 2: Fit LDA model, extract beta and gamma tibbles. The LDA algo draws a
# random sample of parameter values from a Dirichlet dist (which is a
# multivariate version of the Beta dist). It then uses MLE to find the
# parameters which maximize the prob of observing our corpus. For many topics
# and parameter values, this can become hard. The random sampling method is
# called "Gibbs"; this algo compares different probs of topics in each document
# (gamma), and different probs of words in each topic (beta). For example, with
# two topics, it might try the gamma values (0.5, 0.5) vs (0.8, 0.2) for a given
# document. The "alpha" argument controls the prior topic distribution in a
# document. Higher values yield a more even topic distribution, with 1 not
# really favoring balance or imbalance beforehand; the default is 50/k. If you
# know beforehand that the topics per document are quite evenly distributed, try
# using a higher alpha value. The "delta" argument controls the prior word
# distribution in a topic. Higher values yield more even distributions; the
# default is 0.1. The "iter" argument defines how often the random sampling
# process is repeated (higher = better & slower). The "thin" argument indicates
# that results for each n-th iteration should be recorded; the best fitting
# results are finally returned (higher = worse & faster). If the purpose is
# topic discovery, you can keep parameters as below (for classification problems
# this is different, see next section).
model <- LDA(x = dtm,
             k = 2, 
             method = "Gibbs", 
             control = list(alpha = 1, 
                            delta = 0.1, 
                            iter = 2000, 
                            thin = 1,
                            seed = 42))

betas <- tidy(model, matrix = "beta")
gammas <- tidy(model, matrix = "gamma")


# STEP 3: Analyze beta and gamma values.

    # Create word clouds for all j topics. Less precise, but gives nice overview of
    # what topic is about.
    for (j in 1:4) {
    
      word_frequencies <- tidy(model, matrix = "beta") %>% 
                            mutate(n = trunc(beta * 10000)) %>% 
                            filter(topic == j)
      
      wordcloud(word = word_frequencies$term, 
                freq = word_frequencies$n,
                max.words = 20,
                rot.per = 0.3,
                colors = c("DarkOrange", "CornflowerBlue", "DarkRed"), 
                random.order = FALSE, 
                random.color = FALSE)
    }
    
    
    # Probs of words belonging to topics. For many topics, use geom_line instead.
    betas %>% 
      mutate(topic = as.factor(topic)) %>% 
      ggplot(aes(x = term, y = beta)) + 
      geom_col(aes(fill = topic), position = position_dodge()) +
      theme(axis.text.x = element_text(angle=45))
      
    # Probs of topics belonging to documents. For many topics, use geom_line instead.
    gammas %>% 
      mutate(topic = as.factor(topic)) %>% 
      ggplot(aes(x = document, y = gamma)) + 
        geom_col(aes(fill = topic),  position = position_dodge()) +
        theme(axis.text.x = element_text(angle=45))
    
    
    # Get beta values by word.
    betas %>% 
      spread(term, beta)
    
    # Get gamma values by topic.
    gammas %>% 
      spread(topic, gamma)
    
    
    # Prob of term "blah" belonging to topic.
    tidy(model, matrix = "beta") %>%
      filter(term == "blah")
    
    # Prob of topics for document k.
    tidy(model, matrix = "gamma") %>%
      filter(document == k)



########################################
##            Classification          ##
########################################

# Many areas of TM are concerned topic discovery, i.e. what topics are contained
# in a corpus. This is what we looked at so far. However, TM is also used in
# classification problems. One example is named-entity recognition, in which we
# try to predict the meaning of terms, or "entities" based on context, or "word
# windows". An entity is a personal noun (capitalized word). The n words before
# and after an entity are called "word window". For example, consider the string
# "attention of Megara was turned". Then "Megara" is the entity and "attention
# of was turned" could be the word window.


# STEP 1: Extract relevant text. First, write a regex which matches all "n_words
# + entity + n_words" patterns in the text. The number of words before and after
# the entity depends on the text. Setting it low could make predictions worse
# later on, while setting it too high could yield fewer matches (also impacts
# predictions). The regex below will match 2 lowercase words, followed by one or
# a capitalized word (the entity), followed by another 2 lower-case words.
# Second, use this regex and the text as shown below to get a vector of whatever
# matches the pattern (no, str_extract doesn't work). Check how many matches you
# got.
pattern_all <- "( [a-z]+){2}( [A-Z][a-z]+)( [a-z]+){2}"
win_ent_win <- unlist(regmatches(text, gregexpr(pattern_all, text)))
length(win_ent_win)


# STEP 2: Prepare relevant text. First, extract the entities from the patterns
# you found. You can use the same code as before, but using the sub-part of the
# regex matching entities. Second, extract the word windows and append _L1, _L2,
# ... to each word in a given word window. This is done with gsub() by first
# removing entities using the entity-regex and then appending the labels using
# the sub-regex for windows from before. Third, use the entities and windows to
# create a dataframe. This will be our new corpus, with entities serving as
# document id's and windows as text. Use summarize() to combine the texts for
# each entity.
pattern_ent <- "( [A-Z][a-z]+)"
ent <- unlist(regmatches(win_ent_win, gregexpr(pattern_ent, win_ent_win)))

win <- gsub(pattern_ent, "", win_ent_win)
pattern_win <- "([a-z]+) ([a-z]+) ([a-z]+) ([a-z]+)"
label_win <- "\\1_L1 \\2_L2 \\3_R1 \\4_R2"
win <- gsub(pattern_win, label_win, win)

corpus <- data.frame("doc_id" = ent, 
                     "text" = context, 
                     stringsAsFactors = FALSE) %>% 
          group_by(doc_id) %>% 
          summarize(doc = paste(text, collapse = " "))


# STEP 3: Create train/test set. This is done by drawing random subsamples of
# documents (=entities) from our new corpus.
train_ind <- sample.int(n = nrow(corpus), 
                        size = 0.9*nrow(corpus), 
                        replace = FALSE)
train <- corpus[train_ind,]
test <- corpus[-train_ind,]
  
  
# STEP 4: Train topic model using training data. Create a dtm as usual, then fit
# the topic model using LDA(). Unlike in topic discovery problems, you need to
# pay more attention to the model parameters. Try different values to see what
# makes sense.
train_dtm <- train %>% 
              unnest_tokens(input = doc, output = word) %>% 
              count(doc_id, word) %>% 
              cast_dtm(document = doc_id, term = word, value = n)

train_mod <- LDA(x = train_dtm, k = 2, method = "Gibbs",
                 control = list(alpha = 1, seed = 42,
                                iter = 1000, thin = 1))


# STEP 5: Align test data. Before using the model on train set, we must align it
# with the test set. The LDA algo iterates over the dtm column indices and
# counts. Two dtm's with different terms but same number of columns appear the
# same to the algo, so we need to make sure that the test-dtm and train-dtm have
# the same format. First, drop words from the test data that weren't also used
# when the training model. Use tidy() to extract the model's terms, then
# right_join() them to the tokenized and counted test data. Second, deal with
# the NAs resulting from the join. NAs in the n column can be replaced by 0,
# while NAs in the document id column are replaced by the first non-NA id (must
# have an actual name used by the model). The output can then be turned into a
# dtm. Note that the replacements do not affect the prediction, since the algo
# won't use 0-entries in the test-dtm anyway.
model_vocab <- tidy(train_mod, matrix = "beta") %>% 
                  select(term) %>% 
                  distinct()
test_table <- test %>% 
                unnest_tokens(input = doc, output = word) %>% 
                count(doc_id, word) %>%
                right_join(model_vocab, by = c("word"="term"))

# Prepare a document-term matrix
test_dtm <- test_table %>% 
              arrange(desc(doc_id)) %>% 
              mutate(doc_id = ifelse(is.na(doc_id), first(doc_id), doc_id),
                     n = ifelse(is.na(n), 0, n)) %>% 
              cast_dtm(document = doc_id, term = word, value = n)


# STEP 6: Test model. Use posterior() on the model and the test-dtm to get
# predictions. You need to verify manually whether predictions are accurate
# (just look at a few entries).
results <- posterior(object=train_mod, newdata=test_dtm)
results$topics


# REMARK 1: Parameter tuning. There are two ways to choose the optimal number of
# topics. The first one is qualitative: we fit a model and check if the words
# assigned to topics make sense; and looking at the words assigned to topics. If
# a topics contains words that are unrelated, use more topics; if similar words
# are spread across topics, use fewer topics. The second approach is
# quantitative: We fit models with different specifications and check which
# optimizes log-likelihood and perplexity. Log-likelihood measures how
# parameters given the observed data (higher=better), while perplexity measures
# the model's "surprise" at data (lower=better). The qualitative approach makes
# sense in topic discovery, the quantitative approach is better for
# classification problems. The latter is shown below; run the code and use the
# elbow-method to identify the k which yields the best log-likelihood or
# perplexity.
mod_log_lik <- numeric(14)
mod_perplexity <- numeric(14)

for(i in 2:15) {
  
  model <- LDA(train_dtm, k = i, method = "Gibbs",
               control = list(alpha = 1, iter = 1000, seed = 42, thin = 1))
  
  mod_log_lik[i] <- logLik(model)
  mod_perplexity[i] <- perplexity(model)
  
}

plot(x = 2:15, 
     y = mod_log_lik,
     xlab = "Number of Clusters, k", 
     ylab = "Log-Likelihood", 
     type = "o")
plot(x = 2:15, 
     y = mod_perplexity,
     xlab = "Number of Clusters, k", 
     ylab = "Perplexity Score", 
     type = "o")


# REMARK 2: Long texts. If you only have text without documents, or the text of
# each document is very long, split up the text into new (smaller) documents.
# This requires only a small adjustment in the workflow. After tokenizing,
# create a variable equal to the row number, and another one equal to the row
# number modulo-divided by the number of words per new document plus 1. The
# number of words should be long enough to capture events or scenes in the plot
# (for novels: 1000+ words). We add 1 since otherwise the first 999 rows (for
# 1000 words) would have a document number of 0.
t <- unnest_tokens(data, 
                   input = text, 
                   output = word) %>% 
      mutate(word_index = 1:n(),
             document_number = word_index %/% 1000 + 1)




