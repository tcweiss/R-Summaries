
###############################################
###############################################
###                                         ###
###              SENTIMENT ANALYSIS         ###
###                                         ###
###############################################
###############################################

library(qdap)
library(qdapDictionaries)
library(ggthemes)
library(radarchart)
library(tidyverse)
library(tidytext)

########################################
##               Polarity             ##
########################################

df <- tibble("person" = c("Nick", "Jonathan", "Martijn", "Nicole",
                          "Nick", "Jonathan", "Martijn", "Nicole"),
             "text" = c("DataCamp courses are the best", "I like talking to students",
                        "Other online data science curricula are boring.", "What is for lunch?",
                        "DataCamp has lots of great content!", "Students are passionate and are excited to learn",
                        "Other data science curriculum is hard to learn and difficult to understand",
                        "I think the food here is good."))

# Compute polarity. Polarity measures how positive or negative a text is. First,
# each word in a sentence is classified as "polarized term", "valence shifter",
# "negator" or "neutral". Polarized terms are positive or negative words
# associated with emotions (e.g. "amazing" or "ugly"); valence shifters are
# words which amplify or de-amplify emotions (e.g. "very" or "barely"); negators
# invert emotions (e.g. "not"); neutral words do not convey any emotions.
# Second, each word is assigned a score based on its type. Polarized terms count
# as 1 or -1 (if positive/negative); valence shifters as 0.8 or -0.8; negators
# switch the sign of terms that follow them; neutral words count as 0. Third,
# the values are summed and divided by the root of the total number of words in
# a sentence to get the polarity score. For example, "This (0) book (0) is (0)
# not (*switch sign*) very (0.8) interesting (1)" has a polarity score of
# -1.8/sqrt(6). 
# To compute it, start with a tibble and pipe it to polarity() using the dollar
# pipe. The first argument is the text column, the second one is any (potential)
# other column to group by. The other four arguments can be used to set polarity
# terms, negators and valence shifters; the default is an internal dictionary,
# but you can also add own terms (see below). The output is a table with summary
# stats on sentences, words and polarity. Use counts() on the results to get a
# (potentially grouped) table with word counts, positive words, negative words,
# polarity. Use plot() on the results to get (potentially grouped) plot of
# polarity by sentence and polarity-boxplots.
pol_person <- df %$% 
  polarity(text.var = text, 
           grouping.var = person,
           polarity.frame = key.pol,
           negators = negation.words,
           amplifiers = amplification.words,
           deamplifiers = deamplification.words)
counts(pol_person)
plot(pol_person)


# Add terms to polarity dictionary. Depending on the text type, you may need to
# add terms to the default dictionary. Do this using sentiment_frame and use the
# resulting dataframe to compute the scores.
custom_pol <- sentiment_frame(positive.words, c(negative.words, "nagger", "shite"))
df %$% 
  polarity(text.var = text,
           polarity.frame = custom_pol)



########################################
##          SA with Tidytext          ##
########################################

# Tokenize data, count and create dtm.
dtm <- unnest_tokens(text, 
                     input = text,
                     output = "word") %>% 
  count(doc_id, word) %>% 
  cast_dtm(doc_id, word, n)

# Get from dtm to count table again.
tokens <- as.matrix(dtm) %>% 
  tidy()

# Work with bing (positive/negative). Add bing dictionary, count by sentiment
# and document, spread (=pivot_wider) with "sentiment" as key and n as "values"
# so each sentiment has its own column. This gives a tibble with three columns:
# "doc_id", "positive" and "negative" (the latter two count the positive and
# negative terms per document). Define a new variable as the difference between
# "positive" and "negative" values. Finally, plot the difference by document id
# to see the net sentiment per document.
tokens %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment, doc_id) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(diff = positive-negative) %>% 
  ggplot(., aes(doc_id, diff)) + 
  geom_smooth()   



# Work with afinn (-5:5). Add afinn dictionary, count by value and document,
# group by document and define new variable as total value per document.
# Finally, plot the total value by document.
tokens %>% 
  inner_join(get_sentiments("afinn")) %>% 
  count(value, doc_id) %>%
  group_by(doc_id) %>% 
  summarize(total_value = sum(value * n)) %>% 
  ggplot(., aes(doc_id, total_value)) + 
  geom_smooth() 


# Work with nrc (multiple emotions). Count by document and term, add nrc
# dictionary, then remove "positive" and "negative" sentiments. The remaining
# terms form "Plutchkin's wheel of emotion". Group by by "sentiments" and define
# new variable as total count per sentiment. Finally, plot the total count by
# sentiments.
tokens %>% 
  count(doc_id, term) %>%
  inner_join(get_sentiments("nrc")) %>% 
  filter(!sentiment %in% c("positive", "negative")) %>%
  group_by(sentiment) %>% 
  summarize(total_count = sum(n)) %>% 
  ggplot(., aes(x = sentiment, y = total_count)) +
  geom_col()


########################################
##             Visualization          ##
########################################

# Good visual means: Simple to interpret, confirms or highlights other data
# aspects (only more stuff if it adds insight), provide context to audience,
# uses appropriate plot type. Avoid word clouds. Here are some good approaches to
# visualizations. All approaches start with the tokenized and counted data, i.e.
# a tibble with columns "doc_id", "word" and "n".

# Polarity over time. This shows how the sentiment in document develops. Add the
# bing dictionary, count by sentiment and document, then reshape to long format
# so each sentiment has its own column. Add two new columns: the difference
# between positive and negative counts (polarity), and the row number
# (line_number). Plot polarity by line_number using geom_smooth.
tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, doc_id) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate("polarity" = positive - negative,
         "line_number" = row_number()) %>% 
  ggplot(., aes(line_number, polarity)) +
    geom_smooth() +
    geom_hline(yintercept = 0, color = "red") +
    ggtitle("Chronological Polarity")

# Term frequency barplot. This gives an overview of the most common positive and
# negative terms in a document. Add bing dictionary, count by "sentiment" and
# term and set the (already existing) "n" column as weight, reshape to long by
# sentiment and the (new) "n" column, add a new column defined as difference
# between "positive" and "negative" counts (polarity), filter for only rows
# absolute value of at least 50 in "polarity" (to focus on the most relevant
# terms), and add a variable defined as "positive" if "polarity" is >0 and
# "negative" if <0. Plot "polarity" by "term" (reordered by "polarity") colored
# by the new variable using geom_col().
tokens %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, wt = n) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative) %>% 
  filter(abs(polarity) >= 3) %>% 
  mutate(pos_or_neg = ifelse(polarity > 0, "positive", "negative")) %>% 
  ggplot(., aes(reorder(word, polarity), polarity, fill = pos_or_neg)) +
  geom_col() + 
  ggtitle("Sentiment Word Frequency") + 
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 50, vjust = -0.1))

# Stacked positive/negative barplot by document. This shows how much of a each
# document is made up by positive or negative words.  Add nrc dictionary, filter
# sentiment for only "positive" or "negative", count by document and sentiment,
# group by document and add a new variable defined as the per document
# percentages of positive and negative terms. Plot percentages by document
# filled by sentiment (positive/negative) using geom_col.
tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(grepl("positive|negative", sentiment)) %>% 
  count(doc_id, sentiment) %>%
  group_by(doc_id) %>% 
  mutate(perc = 100 * n / sum(n)) %>% 
  ggplot(., aes(doc_id, perc, fill = sentiment)) +  
  geom_col()


# Density plot of overall sentiment score by document. This is better than
# histograms, since we don't have to choose an arbitrary bin size. Add afinn
# dictionary and plot using geom_density.
tokens %>% 
  inner_join(get_sentiments("afinn")) %>%
  ggplot(., aes(value, color = doc_id)) + 
  geom_density(alpha = 0.3) + 
  theme_gdocs() +
  ggtitle("AFINN Score Density")


# Box plot of polarity by document. Good to show distribution of sentiment
# values. Start with the tokenized data by sentences (not word), but do not
# count it. Add a new variable, defined as the polarity of each sentence using
# polarity(). Plot using geom_boxplot.
tokens %>% 
  mutate(polarity = polarity(tokens$sentence)[["all"]][["polarity"]]) %>% 
  ggplot(., aes(x = doc_id, y = polarity)) +
    geom_boxplot(aes(fill = doc_id)) +
    geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 0.02) +
    theme_gdocs() +
    ggtitle("Document Polarity")


# Create radarchart of emotions. This is useful for showing multiple dimensions
# (such as emotions) while avoiding overplotting. Add the nrc dictionary, filter
# sentiment for anything but "positive" or "negative" (will give Plutchkin's
# wheel of emotion), count by sentiment. Finally, plot using charJSRadar().
tokens %>% 
  inner_join(get_sentiments("nrc")) %>%
  filter(!grepl("positive|negative", sentiment)) %>% 
  count(sentiment) %>% 
  chartJSRadar()


# Create treemap by multiple dimensions. Within a treemap each individual box
# represents a document such as a tweet. Documents are grouped in some manner
# such as author. The size of each box is determined by a numeric value such as
# number of words or letters. The individual colors are determined by a
# sentiment score. This means that your tokenized data should have additional
# variables for grouping groups, e.g. author or publisher. Add the afinn
# dictionary, group by one or two groups you are interested in, and compute the
# mean afinn value. Add a variable defined as the number of rows (words) by
# document. Plot using treemap(), with the "index" argument set equal to the
# number of groups you grouped by before computing mean afinn values.
doc_length <- tokens %>%
  count(doc_id)

tokens %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(author, publisher) %>%
  summarize(mean_value = mean(value)) %>% 
  inner_join(doc_length) %>% 
  treemap(.,
          index = c("author", "publisher"),
          vSize = "n",
          vColor = "mean_value",
          type = "value",
          title = "Document Sentiment Scores")



########################################
##               Workflow             ##
########################################

# Stick to the following steps when doing text mining.

# 1) Define the problem & specific goals. This means writing down a specific
# goal that is achievable with text mining techniques. Tips: Be precise, avoid a
# "scope creep", iterate and try new methods/dicts to ensure consistency.

# 2) Identify text. This means finding the right sources, understanding the way
# language is used, and doing some EDA using polarity() and simple plotting (see
# code below). Tips: Find appropriate sources (for stocks, Wikipedia is less useful
# than stock forums), stick to the rules w.r.t web scraping, become familiar
# with the source's tone/nuance/vocabulary.
pol <- polarity(data)
summary(pol$all$polarity)
ggplot(pol$all, aes(x = polarity, y = ..density..)) +
  geom_histogram(binwidth = 0.25, fill = "#bada55", colour = "grey60") +
  geom_density(size = 0.75) +
  theme_gdocs()

# 3) Organize the text. This means preparing a tibble - tokenize, remove stop
# words, add a dictionary, count by some columns (depends on dictionary), and
# create a column measuring sentiment (depends on dictionary). The code in the
# sections before shows what makes sense for each dictionary.

# 4) Extract features. This means doing some more counting and adding some other
# polarity measures, depending on the problem.

# 5) Analyze. This  goes hand-in-hand with step 4). After you extracted
# something relevant, create visuals and compute summary statistics.

# 6) Draw conclusion / reach insights. Tie the entire process together with the
# original problem statement.





