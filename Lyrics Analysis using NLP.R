# ---
# title: "Lyrics Analysis using NLP in R"
# author: "Dhrubasattwata Roy Choudhury"
# ---

### Import the packages
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(widyr)
library(tidyr)
library(wordcloud2) #creative visualizations
library(tm) #text cleaning
library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable)
library(ggrepel)
library(circlize) #Visualizations - chord diagram
library(memery) #Memes - images with plots
library(magick) #Memes - images with plots (image_read)
library(yarrr)  #Pirate plot
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams
library(radarchart)
library(topicmodels)
library(plotly)
library(cleanNLP)
library(mlr)

## Dataset
johnmayer <- read.csv("C:/Users/Dhruba/Desktop/GitHub/R - GitHub/data/johnmayer_music.csv", stringsAsFactors = FALSE)
glimpse(johnmayer[10,])  

dim(johnmayer)

str(johnmayer[10,]$lyrics, nchar.max = 400)

## Data Pre-processing
# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# fix (expand) contractions
johnmayer$lyrics <- sapply(johnmayer$lyrics, fix.contractions)


# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
johnmayer$lyrics <- sapply(johnmayer$lyrics, removeSpecialChars)


# convert everything to lower case
johnmayer$lyrics <- sapply(johnmayer$lyrics, tolower)

str(johnmayer[10,]$lyrics, nchar.max = 400)

summary(johnmayer)

## Decade Column
#Create the decade column
johnmayer <- johnmayer %>%
  mutate(decade = 
                 ifelse(johnmayer$year %in% 1990:1999, "1990s", 
                    ifelse(johnmayer$year %in% 2000:2009, "2000s", 
                        ifelse(johnmayer$year %in% 2010:2019, "2010s", 
                                              "NA"))))

# Create the Chart_level
johnmayer <- johnmayer%>%
  mutate(chart_level = 
           ifelse(johnmayer$peak %in% 1:20, "Top 20", 
                  ifelse(johnmayer$peak %in% 21:100, "Top 100", "Uncharted")))


#create binary field called charted showing if a song hit the charts at all
johnmayer <- johnmayer %>%
  mutate(charted = 
           ifelse(johnmayer$peak %in% 1:100, "Charted", "Uncharted"))

#save the new dataset to .csv for use in later tutorials
#write.csv(johnmayer, file = "C:/Users/Dhruba/Desktop/GitHub/R - GitHub/data/johnmayer_music_new.csv")

### Visualizations

#define theme
my_colors <- c("midnightblue", "orangered4", "darkgreen", "burlywood3", "cyan4")

theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

# Song Stats: Decades and Popularity

johnmayer %>%
  filter(decade != "NA") %>%
  group_by(decade, charted) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = charted), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.6),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual("legend", values = c("Charted" = "darkgreen", "Uncharted" = "cyan4")) +
  ggtitle("John Mayer Songs Count") +
  labs(x = NULL, y = "Number of Songs")

## Song Stats: Popular vs Very Popular
charted_songs_over_time <- johnmayer %>%
  filter(peak > 0) %>%
  group_by(decade, chart_level) %>%
  summarise(number_of_songs = n())

charted_songs_over_time %>% 
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = chart_level), stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual("legend", values = c("Top 20" = "darkgreen", "Top 100" = "cyan4")) +
  labs(x = NULL, y = "John Mayer Songs Count") +
  ggtitle("Popular vs Very Popular Songs")

## Song-Stats: Overall Dataset
johnmayer %>%
  group_by(decade, chart_level) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() +
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = chart_level), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual("legend", values = c("Top 20" = "darkgreen", "Top 100" = "cyan4", "Uncharted" = "orangered3")) +
  labs(x = NULL, y = "John Mayer Songs Count") +
  ggtitle("Overall Song Statistics")

## Best Songs of John Mayer's Career

johnmayer %>%
  filter(peak <= "20") %>%
  select(year, song, peak) %>%
  arrange(year) %>%
  mutate(year = color_tile("lightblue", "lightgreen")(year)) %>%
  mutate(peak = color_tile("lightgreen", "lightgreen")(peak)) %>%
  kable("html", escape = FALSE, align = "c", caption = "John Mayer's Most Popular Songs") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)

### Text Analytics
undesirable_words <- c("prince", "chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")

head(sample(stop_words$word, 15), 15)

# Unnest and Remove Stop words, Undesirable words and Short words

johnmayer_words_filtered <- johnmayer %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

class(johnmayer_words_filtered)
dim(johnmayer_words_filtered)

### Word Frequency
full_word_count <- johnmayer %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song,chart_level) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

full_word_count[1:10,] %>%
  ungroup(num_words, song) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(song = color_tile("lightblue","lightblue")(song)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Highest Word Count for Songs") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)


# Histogram
full_word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words, fill = chart_level )) +
  scale_fill_manual("legend", values = c("Top 20" = "darkgreen", "Top 100" = "cyan4", "Uncharted" = "orangered3")) +
  ylab("John Mayer Songs Count") + 
  xlab("Word Count/ Song") +
  ggtitle("Distribution of Word Counts") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

# Top Words

johnmayer_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[3]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Prince Lyrics")

# Popular Words (10 most popular words by Type)

popular_words <- johnmayer_words_filtered %>% 
  group_by(chart_level) %>%
  count(word, chart_level, sort = TRUE) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(chart_level,n) %>%
  mutate(row = row_number()) 

popular_words %>%
  ggplot(aes(row, n, fill = chart_level)) +
  geom_col() +
  labs(x = NULL, y = "John Mayer Sougs Count") +
  ggtitle("Popular Words by level of Popularity") +
  scale_fill_manual(values = c("Top 20" = "darkgreen", "Top 100" = "cyan4", "Uncharted" = "orangered3")) +
  facet_wrap(~chart_level, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words$row, # notice need to reuse data frame
    labels = popular_words$word) +
  coord_flip()

# Timeless Words

timeless_words <- johnmayer_words_filtered %>% 
  filter(decade != 'NA') %>%
  group_by(decade) %>%
  count(word, decade, sort = TRUE) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(decade,n) %>%
  mutate(row = row_number()) 

timeless_words %>%
  ggplot(aes(row, n, fill = decade)) +
  geom_col() +
  labs(x = NULL, y = "John Mayer Sougs Count") +
  ggtitle("Timeless Words") +
  scale_fill_manual(values = c("1990s" = "darkgreen", "2000s" = "cyan4", "2010s" = "orangered3")) + 
  theme_lyrics() +  
  facet_wrap(~decade, scales = "free", ncol = 5) +
  scale_x_continuous(  # This handles replacement of row 
    breaks = timeless_words$row, # notice need to reuse data frame
    labels = timeless_words$word) +
  coord_flip()

# Word Length

#unnest and remove undesirable words, but leave in stop and short words
johnmayer_word_lengths <- johnmayer %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song,decade) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  mutate(word_length = nchar(word)) 

johnmayer_word_lengths %>%
  count(word_length, sort = TRUE) %>%
  ggplot(aes(word_length), 
         binwidth = 10) + 
  geom_histogram(aes(fill = ..count..),
                 breaks = seq(1,25, by = 2), 
                 show.legend = FALSE) + 
  xlab("Word Length for John Mayer Songs") + 
  ylab("Word Count for John Mayer Songs") +
  ggtitle("Word Length Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())

# Lexical Diversity
lex_diversity_per_year <- johnmayer %>%
  filter(decade != "NA") %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song,year) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) 

diversity_plot <- lex_diversity_per_year %>%
  ggplot(aes(year, lex_diversity)) +
  geom_point(color = "cyan4",
             alpha = .2, 
             size = 5, 
             position = "jitter") + 
  stat_smooth(color = "blue", se = FALSE, method = "lm") +
  geom_smooth(aes(x = year, y = lex_diversity), se = FALSE,
              color = "darkgreen", lwd = 2) +
  ggtitle("Lexical Diversity") +
  xlab("") + 
  ylab("") +
  scale_color_manual(values = my_colors) +
  theme_classic() + 
  theme_lyrics()

diversity_plot

# Lexical Density

lex_density_per_year <- johnmayer %>%
  filter(decade != "NA") %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song,year) %>%
  summarise(lex_density = n_distinct(word)/n()) %>%
  arrange(desc(lex_density))

density_plot <- lex_density_per_year %>%
  ggplot(aes(year, lex_density)) + 
  geom_point(color = "cyan4",
             alpha = .2, 
             size = 5, 
             position = "jitter") + 
  stat_smooth(color = "blue", 
              se = FALSE, 
              method = "lm") +
  geom_smooth(aes(x = year, y = lex_density), 
              se = FALSE,
              color = "darkgreen", 
              lwd = 2) +
  ggtitle("Lexical Density") + 
  xlab("") + 
  ylab("") +
  scale_color_manual(values = my_colors) +
  theme_classic() + 
  theme_lyrics()

density_plot

# Chart History

chart_history <- johnmayer %>%
  filter(peak > 0) %>%
  group_by(year, chart_level) %>%
  summarise(number_of_songs = n()) %>%
  ggplot(aes(year, number_of_songs)) + 
  geom_point(color = "cyan4",
             alpha = .2, 
             size = 5, 
             position = "jitter") +
  geom_smooth(aes(x = year, y = number_of_songs), 
              se = FALSE, 
              method = "lm", 
              color = "blue" ) +
  geom_smooth(aes(x = year, y = number_of_songs), 
              se = FALSE,
              color = "darkgreen", 
              lwd = 2) +
  ggtitle("Chart History") +
  xlab("") + 
  ylab("") +
  scale_color_manual(values = my_colors) +
  theme_classic() + 
  theme_lyrics()

# Comparison
grid.arrange(diversity_plot, density_plot, chart_history, ncol = 3)

# TF-IDF
popular_tfidf_words <- johnmayer %>%
  unnest_tokens(word, lyrics) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  count(chart_level, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, chart_level, n)

head(popular_tfidf_words)

top_popular_tfidf_words <- popular_tfidf_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(chart_level) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(chart_level, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, 
             fill = chart_level)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words using TF-IDF by Chart Level") +
  scale_fill_manual(values = c("Top 20" = "darkgreen", "Top 100" = "cyan4", "Uncharted" = "orangered3")) + 
  theme_lyrics() +  
  facet_wrap(~chart_level, ncol = 3, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = top_popular_tfidf_words$row, # notice need to reuse data frame
    labels = top_popular_tfidf_words$word) +
  coord_flip()

# TF=IDF across time

tfidf_words_decade <- johnmayer %>%
  unnest_tokens(word, lyrics) %>%
  distinct() %>%
  filter(!word %in% undesirable_words & decade != 'NA') %>%
  filter(nchar(word) > 3) %>%
  count(decade, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, decade, n) %>%
  arrange(desc(tf_idf))

top_tfidf_words_decade <- tfidf_words_decade %>% 
  group_by(decade) %>% 
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(decade, tf_idf) %>%
  mutate(row = row_number())

top_tfidf_words_decade %>%
  ggplot(aes(x = row, tf_idf, fill = decade)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words (top 10) using TF-IDF by Decade") +
  scale_fill_manual(values = c("1990s" = "darkgreen", "2000s" = "cyan4", "2010s" = "orangered3")) +
  theme_lyrics() +  
  facet_wrap(~decade, 
             ncol = 3, nrow = 2, 
             scales = "free") +
  scale_x_continuous(  # this handles replacement of row 
    breaks = top_tfidf_words_decade$row, # notice need to reuse data frame
    labels = top_tfidf_words_decade$word) +
  coord_flip()



# Sentiment Analysis

## Visualization defaults
#Customize ggplot2's default theme settings
#This tutorial doesn't actually pass any parameters, but you may use it again in future tutorials so it's nice to have the options
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = aticks, #Set axis ticks to on or off
        panel.grid.minor = pgminor, #Turn the minor grid lines on or off
        legend.title = lt, #Turn the legend title on or off
        legend.position = lp) #Turn the legend on or off
}

#Customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

# Dataset
johnmayer_data <- johnmayer
glimpse(johnmayer_data)

# Data Cleaning
undesirable_words

johnmayer_tidy <- johnmayer_data %>%
  unnest_tokens(word, lyrics) %>% #Break the lyrics into individual words
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3) %>% #Words like "ah" or "oo" used in music
  anti_join(stop_words) #Data provided by the tidytext package

glimpse(johnmayer_tidy)

# Descriptive Statistics
# Word Count Per Song by Popularity

word_summary <- johnmayer_tidy %>%
  mutate(decade = ifelse(is.na(decade),"NONE", decade)) %>%
  group_by(decade, song) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(song, Released = decade, Charted = charted, word_count) %>%
  distinct() %>% #To obtain one record per song
  ungroup()

pirateplot(formula =  word_count ~ Released + Charted, #Formula
           data = word_summary, #Data frame
           xlab = NULL, ylab = "Song Distinct Word Count", #Axis labels
           main = "Lexical Diversity Per Decade", #Plot title
           pal = c("cyan4","orangered2","darkgreen"), #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size

# Song Count Per Year

songs_year <- johnmayer_data %>%
  select(song, year) %>%
  group_by(year) %>%
  summarise(song_count = n())

id <- seq_len(nrow(songs_year))
songs_year <- cbind(songs_year, id)
label_data = songs_year
number_of_bar = nrow(label_data) #Calculate the ANGLE of the labels
angle = 90 - 360 * (label_data$id - 0.5) / number_of_bar #Center things
label_data$hjust <- ifelse(angle < -90, 1, 0) #Align label
label_data$angle <- ifelse(angle < -90, angle + 180, angle) #Flip angle

ggplot(songs_year, aes(x = as.factor(id), y = song_count)) +
  geom_bar(stat = "identity", fill = "darkgreen" ) +
  geom_text(data = label_data, aes(x = id, y = song_count + 10, label = year, hjust = hjust), color = "black", alpha = 0.6, size = 3, angle =  label_data$angle, inherit.aes = FALSE ) +
  coord_polar(start = 0) +
  ylim(-20, 80) + #Size of the circle
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-4,4), "in"),
        plot.title = element_text(margin = margin(t = 10, b = -10)))

# Charted Songs By Decade

decade_chart <-  johnmayer_data %>%
  filter(decade != "NA") %>% #Remove songs without release dates
  count(decade, charted)  #Get SONG count per chart level per decade. Order determines top or bottom.

circos.clear() #Very important - Reset the circular layout parameters!
grid.col = c("1970s" = my_colors[1], "1980s" = my_colors[2], "1990s" = my_colors[3], "2000s" = my_colors[4], "2010s" = my_colors[5], "Charted" = "grey", "Uncharted" = "grey") #assign chord colors
# Set the global parameters for the circular layout. Specifically the gap size
circos.par(gap.after = c(rep(5, length(unique(decade_chart[[1]])) - 1), 15,
                         rep(5, length(unique(decade_chart[[2]])) - 1), 15))

chordDiagram(decade_chart, grid.col = grid.col, transparency = .2)
title("Relationship Between Popularity and Decade")

# Lexicons and Lyrics
afinn <- tidytext::get_sentiments("afinn") 
bing <- tidytext::get_sentiments("bing") 
nrc <- tidytext::get_sentiments("nrc") 

afinn <- afinn %>%
  mutate(lexicon = "AFINN") %>%
  mutate(sentiment = ifelse(value>=0, "positive","negative"))

afinn <- select(afinn,c(1,4,3))

bing <- bing %>%
  mutate(lexicon = "bing")
nrc <- nrc %>%
  mutate(lexicon = "nrc")

sentiments <- rbind(afinn,bing,nrc)

new_sentiments <- sentiments %>% #From the tidytext package
  group_by(lexicon) %>%
  mutate(words_in_lexicon = n_distinct(word)) %>%
  ungroup()


new_sentiments %>%
  group_by(lexicon, sentiment, words_in_lexicon) %>%
  summarise(distinct_words = n_distinct(word)) %>%
  ungroup() %>%
  spread(sentiment, distinct_words) %>%
  mutate(lexicon = color_tile("lightblue", "lightblue")(lexicon),
         words_in_lexicon = color_bar("lightgreen")(words_in_lexicon)) %>%
  my_kable_styling(caption = "Word Counts by Lexicon")


# Match Dot Common

johnmayer_tidy %>%
  mutate(words_in_lyrics = n_distinct(word)) %>%
  inner_join(new_sentiments) %>%
  group_by(lexicon, words_in_lyrics, words_in_lexicon) %>%
  summarise(lex_match_words = n_distinct(word)) %>%
  ungroup() %>%
  mutate(total_match_words = sum(lex_match_words), #Not used but good to have
         match_ratio = lex_match_words / words_in_lyrics) %>%
  select(lexicon, lex_match_words,  words_in_lyrics, match_ratio) %>%
  mutate(lex_match_words = color_bar("lightblue")(lex_match_words),
         lexicon = color_tile("lightgreen", "lightgreen")(lexicon)) %>%
  my_kable_styling(caption = "Lyrics Found In Lexicons")


# Create Sentiment Dataset
johnmayer_bing <- johnmayer_tidy %>%
  inner_join(get_sentiments("bing"))

johnmayer_nrc <- johnmayer_tidy %>%
  inner_join(get_sentiments("nrc"))

johnmayer_nrc_sub <- johnmayer_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))



nrc_plot <- johnmayer_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 1000)) + #Hard code the axis limit
  ggtitle("John Mayer NRC Sentiment") +
  coord_flip()

nrc_plot

bing_plot <- johnmayer_bing %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 800)) +
  ggtitle("John Mayer Bing Sentiment") +
  coord_flip()

grid.arrange(nrc_plot,bing_plot,ncol=2)

## Positivity and Negativity by Popularity

johnmayer_polarity_chart <- johnmayer_bing %>%
  count(sentiment, chart_level) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative) * 100)

#Polarity by chart
plot1 <- johnmayer_polarity_chart %>%
  ggplot( aes(chart_level, polarity, fill = chart_level)) +
  geom_col() +
  scale_fill_manual(values = my_colors[3:5]) +
  geom_hline(yintercept = 0, color = "red") +
  theme_lyrics() + theme(plot.title = element_text(size = 11)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Polarity By Chart Level")

#Percent positive by chart
plot2 <- johnmayer_polarity_chart %>%
  ggplot( aes(chart_level, percent_positive, fill = chart_level)) +
  geom_col() +
  scale_fill_manual(values = c(my_colors[3:5])) +
  geom_hline(yintercept = 0, color = "red") +
  theme_lyrics() + theme(plot.title = element_text(size = 11)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Percent Positive By Chart Level")

grid.arrange(plot1, plot2, ncol = 2)


# Mood Ring

grid.col = c("1990s" = "cyan4", "2000s" = "darkgreen", "2010s" = "orangered3", "anger" = "grey", "anticipation" = "grey", "disgust" = "grey", "fear" = "grey", "joy" = "grey", "sadness" = "grey", "surprise" = "grey", "trust" = "grey")

decade_mood <-  johnmayer_nrc %>%
  filter(decade != "NA" & !sentiment %in% c("positive", "negative")) %>%
  count(sentiment, decade) %>%
  group_by(decade, sentiment) %>%
  summarise(sentiment_sum = sum(n)) %>%
  ungroup()

circos.clear()
#Set the gap size
circos.par(gap.after = c(rep(5, length(unique(decade_mood[[1]])) - 1), 15,
                         rep(5, length(unique(decade_mood[[2]])) - 1), 15))
chordDiagram(decade_mood, grid.col = grid.col, transparency = .2)
title("Relationship Between Mood and Decade")

# On The Radar: Radar Charts

#Get the count of words per sentiment per year
decade_sentiment_nrc <- johnmayer_nrc_sub %>%
  group_by(decade, sentiment) %>%
  count(decade, sentiment) %>%
  select(decade, sentiment, sentiment_decade_count = n)

#Get the total count of sentiment words per year (not distinct)
total_sentiment_decade <- johnmayer_nrc_sub %>%
  count(decade) %>%
  select(decade, decade_total = n)

#Join the two and create a percent field
decade_radar_chart <- decade_sentiment_nrc %>%
  inner_join(total_sentiment_decade, by = "decade") %>%
  mutate(percent = sentiment_decade_count / decade_total * 100 ) %>%
  select(-sentiment_decade_count, -decade_total) %>%
  spread(decade, percent) %>%
  chartJSRadar(showToolTipLabel = TRUE,
               main = "John Mayer NRC Years Radar")


# Mood of song
johnmayer_nrc %>%
  filter(song %in% "Your Body Is A Wonderland") %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) +
  theme_minimal() + theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  ggtitle("Your Body Is A Wonderland NRC Sentiment") +
  coord_flip()

johnmayer_tidy %>%
  filter(song %in% 'Your Body Is A Wonderland') %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = word, fill = sentiment)) +
  facet_grid(~sentiment) +
  geom_bar() + #Create a bar for each word per sentiment
  theme_lyrics() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) + #Place the words on the y-axis
  xlab(NULL) + ylab(NULL) +
  ggtitle("Your Body Is A Wonderland Sentiment Words") +
  coord_flip()

# Bigrams Per Decade
johnmayer_bigrams <- johnmayer_data %>%
  unnest_tokens(bigram, lyrics, token = "ngrams", n = 2)

bigrams_separated <- johnmayer_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% undesirable_words) %>%
  filter(!word2 %in% undesirable_words)

#Because there is so much repetition in music, also filter out the cases where the two words are the same
bigram_decade <- bigrams_filtered %>%
  filter(word1 != word2) %>%
  filter(decade != "NA") %>%
  unite(bigram, word1, word2, sep = " ") %>%
  inner_join(johnmayer_data) %>%
  count(bigram, decade, sort = TRUE) %>%
  group_by(decade) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(decade, n) %>%
  mutate(row = row_number())

bigram_decade %>%
  ggplot(aes(row, n, fill = decade)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~decade, scales = "free_y") +
  xlab(NULL) + ylab(NULL) +
  scale_x_continuous(  # This handles replacement of row
    breaks = bigram_decade$row, # Notice need to reuse data frame
    labels = bigram_decade$bigram) +
  theme_lyrics() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle("Bigrams Per Decade") +
  scale_fill_manual(values = c("1990s" = "darkgreen", "2000s" = "cyan4", "2010s" = "orangered3")) +
  coord_flip()

# Sentiments and Bigrams
AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE) %>%
  ungroup()

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  theme_lyrics() +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * Number of Occurrences") +
  ggtitle("Polar Sentiment of Words Preceded by Not") +
  coord_flip()
#--------------------------------------------

negation_words <- c("not", "no", "never", "without")

negation_bigrams <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  group_by(word1) %>%
  slice(seq_len(20)) %>%
  arrange(word1,desc(contribution)) %>%
  ungroup()

bigram_graph <- negation_bigrams %>%
  graph_from_data_frame() #From `igraph`

set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(alpha = .25) +
  geom_edge_density(aes(fill = value)) +
  geom_node_point(color = "cyan4", size = 2) + 
  geom_node_text(aes(label = name),  repel = TRUE) +
  theme_void() + theme(legend.position = "none",
                       plot.title = element_text(hjust = 0.5)) +
  ggtitle("Negation Bigram Network")


# Pairwise Comparisons
pwc <- johnmayer_tidy %>%
  filter(n() >= 5) %>%  #High counts
  pairwise_count(word, song, sort = TRUE) %>%
  filter(item1 %in% c("love", "body", "perfect")) %>%
  group_by(item1) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  mutate(row = -row_number()) #Descending order

pwc %>%
  ggplot(aes(row, n, fill = item1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~item1, scales = "free") +
  scale_x_continuous(  #This handles replacement of row
    breaks = pwc$row, #Notice need to reuse data frame
    labels = pwc$item2) +
  theme_lyrics() + theme(panel.grid.major.x = element_blank()) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Pairwise Counts") +
  coord_flip()

johnmayer_tidy %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, song, sort = TRUE) %>%
  filter(item1 %in% c("love", "body", "perfect")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  facet_wrap(~item1, scales = 'free') +
  theme_lyrics() + theme(panel.grid.major.x = element_blank()) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Pairwise Correlation") +
  coord_flip()

### Trying to Model 
### TOPIC MODELLING

word_chart <- function(data, input, title) {
  data %>%
    #set y = 1 to just plot one variable and use word as the label
    ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
    #you want the words, not the points
    geom_point(color = "transparent") +
    #make sure the labels don't overlap
    geom_label_repel(nudge_x = .2,  
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~topic) +
    theme_lyrics() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          #axis.title.x = element_text(size = 9),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = title) +
    #xlab(NULL) + ylab(NULL) +
    #ggtitle(title) +
    coord_flip()
}


## LDA
johnmayer_annotated <- read.csv("C:/Users/Dhruba/Desktop/GitHub/R - GitHub/data/johnmayer_data_annotated.csv", stringsAsFactors = FALSE)
names(johnmayer_annotated) 
table(johnmayer_annotated$upos)

# Set Variables Using NLP
source_tidy <- johnmayer_annotated %>%
  select(song = id, word, lemma, upos) %>%
  filter(upos == "NOUN") %>% #choose only the nouns
  inner_join(johnmayer_tidy, by = c("word", "song")) %>%
  select(song, word, lemma, upos, year) %>%
  mutate(source = "johnmayer")%>%
  mutate(genre = "bluesrock") %>%
  distinct()

source_dtm <- source_tidy %>%
  #filter out some words that exist across themes just for our purposes
  filter(!word %in% c("love", "perfect", "body", "night", "girl")) %>%
  count(song, word, sort = TRUE) %>%
  ungroup() %>%
  cast_dtm(song, word, n)

# Fit the Model and Identify Themes
#Changing these parameters or the source data will cause different results!!
k <- 5 # number of topics
num_words <- 6
seed = 4321
lda <- LDA(source_dtm, k = k, method = "GIBBS", control = list(seed = seed))

top_terms_per_topic <- function(lda_model, num_words) {
  
  #tidy LDA object to get word, topic, and probability (beta)
  topics_tidy <- tidy(lda_model, matrix = "beta")
  
  
  top_terms <- topics_tidy %>%
    group_by(topic) %>%
    arrange(topic, desc(beta)) %>%
    #get the top num_words PER topic
    slice(seq_len(num_words)) %>%
    arrange(topic, beta) %>%
    #row is required for the word_chart() function
    mutate(row = row_number()) %>%
    ungroup() %>%
    #add the word Topic to the topic labels
    mutate(topic = paste("Topic", topic, sep = " "))
  #create a title to pass to word_chart
  title <- paste("LDA Top Terms for", k, "Topics")
  #call the word_chart function you built in prep work
  word_chart(top_terms, top_terms$term, title)
}

top_terms_per_topic(lda, num_words)

### Themes Over Time
p1 <-  johnmayer_tidy %>%
  filter(!year == "NA") %>% #remove songs without years
  filter(word %in% c("fallin", "head", "town","woman","neon","daughters")) %>%
  group_by(year) %>%
  mutate(topic_1_count = n()) %>%
  select(year, topic_1_count) %>%
  distinct() %>%
  ggplot(aes(year, topic_1_count)) + geom_smooth(se = FALSE, col = "darkgreen")

p2 <-  johnmayer_tidy %>%
  filter(!year == "NA") %>% #remove songs without years
  filter(word %in% c("heart", "friends", "loving","trust","weekend","hand")) %>%
  group_by(year) %>%
  mutate(topic_2_count = n()) %>%
  select(year, topic_2_count) %>%
  distinct() %>%
  ggplot(aes(year, topic_2_count)) + geom_smooth(se = FALSE, col = "darkgreen")

p3 <-  johnmayer_tidy %>%
  filter(!year == "NA") %>% #remove songs without years
  filter(word %in% c("world", "water", "found","whiskey","days","train")) %>%
  group_by(year) %>%
  mutate(topic_3_count = n()) %>%
  select(year, topic_3_count) %>%
  distinct() %>%
  ggplot(aes(year, topic_3_count)) + geom_smooth(se = FALSE, col = "darkgreen")

p4 <-  johnmayer_tidy %>%
  filter(!year == "NA") %>% #remove songs without years
  filter(word %in% c("light","tonight","fool","roll","tomorrow","sadness")) %>%
  group_by(year) %>%
  mutate(topic_4_count = n()) %>%
  select(year, topic_4_count) %>%
  distinct() %>%
  ggplot(aes(year, topic_4_count)) + geom_smooth(se = FALSE, col="darkgreen")

p5 <-  johnmayer_tidy %>%
  filter(!year == "NA") %>% #remove songs without years
  filter(word %in% c("time","life","line","living","moving","olivia")) %>%
  group_by(year) %>%
  mutate(topic_5_count = n()) %>%
  select(year, topic_5_count) %>%
  distinct() %>%
  ggplot(aes(year, topic_5_count)) + geom_smooth(se = FALSE, col="darkgreen")

grid.arrange(p1, p2, p3, p4, p5, ncol = 2)

## Song-Stats: Overall Dataset
johnmayer %>%
  group_by(decade, chart_level) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() +
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = chart_level), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual("legend", values = c("Top 20" = "darkgreen", "Top 100" = "cyan4", "Uncharted" = "orangered3")) +
  labs(x = NULL, y = "John Mayer Songs Count") +
  ggtitle("Overall Song Statistics")
