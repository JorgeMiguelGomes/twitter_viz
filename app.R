#import libraries

library(rtweet)
library(igraph)
library(hrbrthemes)
library(ggraph)
library(tidyverse)

# using rtweet query the twitter API for the keyword(s)
# and assign it to a variable

rstats <- search_tweets("Your Keyword Here", n=3000)

# Choose your dataset.
# rtweet_count > 1 will include RTs.
# You can change this number to any other


filter(rstats, retweet_count > 1) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> rt_g


# Now you have a list (rt_g) that you can use to plot the graph

# Feel free to play around with the numeric values :)

# Calculate relationships

V(rt_g)$node_label <- unname(ifelse(degree(rt_g)[V(rt_g)] > 10, names(V(rt_g)), "")) 
V(rt_g)$node_size <- unname(ifelse(degree(rt_g)[V(rt_g)] > 10, degree(rt_g), 100)) 

# Calculate graph. Circular can be TRUE or FALSE
# max.overlaps is used to define if usernames can overlap or not
# repel is used to avoid overlaps between labels
# labs defines the labels of the graph
# You can define your own colors 

ggraph(rt_g, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(edge_width=0.125, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour = "#a0a2a3", 
                  color="#000000", repel = TRUE, max.overlaps=200, family = font_rc, fontface = "bold") +
  coord_fixed() +
  scale_size_area(trans = "sqrt") +
  labs(title = "Your Title Here", subtitle = "Your Subtitle Here " , caption = "dataviz by jmgvostpt") +
  theme_graph(base_family = font_rc) +
  theme(legend.position="none")

# please use it, fork it, update it.
# You can find me on twitter @jmgvostpt (PT) or @JGOMES_EU (EN)
