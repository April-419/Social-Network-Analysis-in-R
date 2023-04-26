# Lab 1a: Descriptive Network Analysis

# Start with a clear environment
rm(list=ls())

list.of.packages <- c("tidytext", "tidygraph","ggraph","igraph","tidyverse","topicmodels")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(readr)
library(tidytext)
library(tidygraph)
library(ggraph)
library(igraph)
library(tidyverse)
library(topicmodels)
library(textstem)

sessionInfo()

######################################################################################
#
# Part I: Network Generation from Text
#
######################################################################################

gpt_text <- read_file("/Users/jianingjin/Desktop/IEMS341/chatgpt.txt")
human_text <- read_file("/Users/jianingjin/Desktop/IEMS341/human.txt")

text = tibble(gpt = gpt_text, human = human_text)

# create edgelist from text skipngrams
df_skip <- text |> 
  pivot_longer(cols= c(gpt, human)) |>
  unnest_tokens(skipgrams, value, token = "skip_ngrams", n = 2, k = 10) |>  
  separate(skipgrams, c("skip_1", "skip_2"), sep = " ", extra = "merge") |> 
  mutate(skip_1 = textstem::lemmatize_words(skip_1),
         skip_2 = textstem::lemmatize_words(skip_2)) |>
  na.omit() |> 
  filter(!skip_1 %in% stop_words$word) |>
  filter(!skip_2 %in% stop_words$word) |>
  filter(skip_1!= skip_2) |> 
  count(name, skip_1, skip_2, sort = TRUE)

df_gpt <-  df_skip |>
  filter(name == "gpt") |> 
  filter(n>2)

df_hmn <-  df_skip |>
  filter(name == "human") |> 
  filter(n>2)

# filter rare word co-occurrences and create a graph object
df_both <- df_skip |> 
  filter(n>2) |>
  select(skip_1,skip_2, name)

# convert dataframe to long format to see all words
df_long <- df_both |> gather(source,word,skip_1:skip_2)

# convert dataframe to long format to see all words
df_long <- df_both |> gather(source,word,skip_1:skip_2)

# create a dataframe which labels the text source of the word (human, gpt, or both)
vertex_labels <- df_long |> distinct(word) |>
  left_join(distinct(df_long |> select(word,name) |> filter(name == "gpt"))) |>
  left_join(distinct(df_long |> select(word,name) |> filter(name == "human")), by = "word") |>
  mutate(source = case_when(is.na(name.x)~"human",is.na(name.y)~"gpt",T~"both")) |>
  select(word,source)

# generate a labeled graph
data_graph <- graph_from_data_frame(df_both, vertices = vertex_labels) |>
  as_tbl_graph()

########################
# Topic Modeling
########################
# first transform the text into a document-term matrix
text_dtm <- text |>
  pivot_longer(cols= c(gpt, human)) |>
  unnest_tokens(word, value) |>
  mutate(word = textstem::lemmatize_words(word)) |> # this line performs lemmatization, standardizing words
  filter(!word %in% stop_words$word) |> # this line removes stop words (insignificant words for analysis)
  count(name, word, sort = TRUE)|>
  cast_dtm(name,word,n)

# perform LDA analysis to group topics
# the number of topics selected was k = 3
text_lda <- LDA(text_dtm, k = 3, control = list(seed = 1234))

# create the topic map
# uses LDA to group words into topics
topic_map <- augment(text_lda, data = text_dtm) |>
  filter(count>2) |>
  select(term, .topic) |>
  distinct() |>
  add_row(term=c("human","gpt"),.topic=0,.before=0) |>
  group_by(term) |>
  mutate(n_topics = row_number()) |>
  filter(n_topics == 1) |>
  ungroup()

# gpt_graph represents the artificial intelligence network
gpt_graph <- df_gpt |> 
  filter(n>2) |>
  select(skip_1,skip_2, name) |>
  graph_from_data_frame() |>
  as_tbl_graph() |>
  left_join(topic_map, by = c("name" = "term")) |>
  mutate(topic = `.topic` |> as_factor())

# hmn_graph represents the collective intelligence network
hmn_graph <- df_hmn |> 
  filter(n>2) |>
  select(skip_1,skip_2, name) |>
  graph_from_data_frame() |>
  as_tbl_graph() |>
  left_join(topic_map, by = c("name" = "term")) |>
  mutate(topic = `.topic` |> as_factor())

########################
# Check the criterion
########################
# check the size of the networks
vcount(data_graph) ## the number of nodes
ecount(data_graph) ## the number of edges

vcount(gpt_graph) ## the number of nodes
ecount(gpt_graph) ## the number of edges

vcount(hmn_graph) ## the number of nodes
ecount(hmn_graph) ## the number of edges

# calculate the density of the networks
graph.density(data_graph)
graph.density(gpt_graph)
graph.density(hmn_graph)

# check if the networks are directed or undirected
is.directed(data_graph)
is.directed(gpt_graph)
is.directed(hmn_graph)

########################
# Save your data       
########################
save.image('Lab_1a.RData')

######################################################################################
#
# Part II: Network Visualization
#
######################################################################################

# Calculate the number of components in the graph
gpt_comp <- components(gpt_graph); gpt_comp
hmn_comp <- components(hmn_graph); hmn_comp

# start with the AI network
gpt_comp <- components(gpt_graph)
giantGraph_gpt <- gpt_graph %>% 
  induced.subgraph(., which(gpt_comp$membership == which.max(gpt_comp$csize)))

# now repeat steps with the collective intelligence network
hmn_comp <- components(hmn_graph)
giantGraph_hmn <- hmn_graph %>% 
  induced.subgraph(., which(hmn_comp$membership == which.max(hmn_comp$csize)))

########################
# Plotting   
########################
## plot the original AI network
plot(gpt_graph, vertex.size = 7, vertex.label = NA)

plot(gpt_graph, vertex.size = 7, vertex.label = NA, 
     layout = layout_nicely(gpt_graph))

## plot the largest component of the AI network
plot(giantGraph_gpt, vertex.size = 7, vertex.label = NA)

plot(giantGraph_gpt, vertex.size = 7, vertex.label = NA, 
     layout = layout_nicely(giantGraph_gpt))

#######################################
# Text & Topic Comparison 
#######################################
data_graph |>
  as_tbl_graph() |>
  ggraph(layout = 'fr')+
  geom_edge_link2(aes())+
  geom_node_label(aes(label = name,colour=source))+ # color nodes by source text
  theme_void()

# plot the artificial intelligence semantic network with node color representing topic
gpt_graph |>
  as_tbl_graph() |>
  ggraph(layout = 'fr')+
  geom_edge_link2(aes())+
  geom_node_label(aes(label = name,colour=topic))+ # color nodes by topic
  theme_void()

# plot the collective intelligence semantic network with node color representing topic
hmn_graph |>
  as_tbl_graph() |>
  ggraph(layout = 'fr')+
  geom_edge_link2(aes())+
  geom_node_label(aes(label = name,colour=topic))+
  theme_void()

