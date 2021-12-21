library(stringr) 
library(tidyr)
library(tidyverse)

# mention network 
df_raw = readr::read_csv("https://raw.githubusercontent.com/eppofahmi/twitpres/master/Data%20Raw/u-eppo%20q-from%20prabowo%20from%202018-09-21%20to%202019-10-20.csv")
glimpse(df_raw)

# mengekstrak username dari kolom full text
# Saya dan keluarga besar Partai @Gerindra turut berduka cita sedalam-dalamnya.....
?str_extract_all
mentioned <- str_extract_all(string = df_raw$full_text,
                             pattern = "(@[[:alnum:]_]*)", simplify = TRUE)
## Googling : extract username from twitter data in R

mentioned <- data.frame(mentioned)
glimpse(mentioned)

# Menggabungkan 2 kolom
mentioned <- mentioned %>%
  unite("mention", sep = " ")

df_raw$mentioned <- paste0(mentioned$mention)
df_raw = df_raw %>%
  select(user_screen_name, mentioned)

colnames(df_raw) = c("sumber","target")
glimpse(df_raw)

df_raw$sumber = paste0("@", df_raw$sumber)
df_raw$stringC = str_count(df_raw$target)

# replace extraspace
library(textclean)

df_raw$target = replace_white(df_raw$target)

df_net = df_raw %>% 
  ungroup() %>% 
  select(-stringC) %>% 
  filter(str_count(target, pattern = "\\S+") >= 1) %>% 
  filter(!str_detect(string = target, pattern = "NA"))

# membuat adjcency list
library(tidytext)

df_net = df_net %>% 
  unnest_tokens(target, target, token = "words", to_lower = FALSE)
df_net$target = paste0("@", df_net$target)

View(df_net)
class(df_net)

# membuat objek graph
library(igraph)
?graph_from_data_frame
net_result = graph_from_data_frame(d = df_net, directed = TRUE)
class(net_result)
plot(net_result)

# mendapatkan nama nodes
V(graph = net_result)$name
name <- data_frame(nodes = V(graph = net_result)$name )

# degree centrality
?centr_degree
degree.cent <- centr_degree(net_result, mode = "all")
degree.cent <- data_frame(degree = degree.cent$res)
centr_result = bind_cols(name, degree.cent)

# closness centr
closeness.cent <- closeness(net_result, mode = "all")
closeness.cent <- tibble(closeness.cent)

centr_result = bind_cols(centr_result, closeness.cent)

# Betweeness
?betweenness
betweenness <- betweenness(graph = net_result)
betweenness = tibble(betweenness)

centr_result = bind_cols(centr_result, betweenness)

# Eigenvector
eigevectore <- eigen_centrality(graph = net_result)
eigenvector <- data_frame(eigen = eigenvector[["vector"]])

centr_result = bind_cols(centr_result, eigenvector)

# modularity
?cluster_walktrap
wc <- cluster_walktrap(graph = graph_sample) 
modularity(wc)
membership(wc)
