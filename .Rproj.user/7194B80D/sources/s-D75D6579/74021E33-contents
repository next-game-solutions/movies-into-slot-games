require(tidyverse)
require(RColorBrewer)
require(proxy)

# ------------------------------- Load data ----------------------------------------

load("./workspaces/game_and_movie_data.RData")


# --------------------- Cluster analysis based on genre tags -----------------------

# Create distance matrix 
# (based on the "extended Dice distance", which was experimentally 
# found to produce an interpretable clustering solution):
d <- genres %>%
  left_join(., 
            distinct(select(games, movie_id, movie_name)), 
            by = "movie_id") %>%
  ungroup()
m_names <- d$movie_name

d <- d %>% 
  ungroup() %>% 
  select(-movie_id, -movie_name) %>% 
  as.matrix()
rownames(d) <- m_names

d <- proxy::dist(d, method = 'eDice')


# Cluster analysis and visualisation of its results:
h <- hclust(d, method = "ward.D2")
hd <- as.dendrogram(h)

clusMember <- cutree(h, 7) # 7 clusters
labelColors <- brewer.pal(7, "Dark2")

# utility function for plotting:
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}

clusDendro <- dendrapply(hd, colLab)

par(mar = c(3, 1, 0, 21), cex = 0.7, font = 3)
plot(clusDendro, ann = FALSE, horiz = TRUE)


# Percentage of genre tags in each cluster:
tmdb_overview %>% 
  select(cluster, genre) %>% 
  unnest(cols = genre) %>% 
  group_by(cluster, genre) %>% 
  count() %>% 
  arrange(cluster, -n) %>% 
  group_by(cluster) %>% 
  mutate(percent = n / sum(n) * 100)


# ----------------- Over-represented directors and top-billed actors ----------------

# Directors and number of films:
(director_counts <- tmdb_credits %>% 
   filter(credits == "crew") %>% 
   unnest(cols = data) %>% 
   group_by(name) %>% 
   count() %>% 
   arrange(-n))

# Percentage of directors with a given number of movies:
director_counts %>% 
  pull(n) %>% 
  table() %>% 
  prop.table() * 100

# Top billing actors and number of films:
(actor_counts <- tmdb_credits %>% 
    filter(credits == "cast") %>% 
    unnest(cols = data) %>% 
    group_by(name) %>% 
    count() %>% 
    arrange(-n))

# Percentage of actors with a given number of movies:
actor_counts %>% 
  pull(n) %>% 
  table() %>% 
  prop.table() * 100

# Movies where top actors appeared:
actor_counts %>% 
  filter(n > 2) %>% 
  left_join(., unnest(filter(tmdb_credits, credits == "cast"), 
                      cols = data),
            by = "name") %>% 
  rename(tmdb_movie_id = movie_id) %>% 
  left_join(., select(games, movie_name, tmdb_movie_id),
            by = "tmdb_movie_id") %>% 
  select(name, movie_name) %>% 
  distinct()


# -------------------------------- MPAA ratings ---------------------------------

mppa_agg <- tmdb_certificates %>% 
  na.omit() %>% 
  filter(mpaa != "NR") %>% 
  group_by(mpaa) %>% 
  count() %>% 
  arrange(-n) %>% 
  ungroup() %>% 
  mutate(this_study = n / sum(n) * 100) %>% 
  mutate(blanchet_2011 = case_when(
    mpaa == "PG-13" ~ 25,
    mpaa == "R" ~ 21,
    mpaa == "PG" ~ 39,
    mpaa == "G" ~ 13
  )) %>% 
  mutate(mpaa = factor(mpaa, ordered = TRUE, 
                       levels = c("G", "PG", "PG-13", "R")))

# plot the result:
mppa_agg %>% 
  select(-n) %>% 
  pivot_longer(cols = c(this_study, blanchet_2011),
               names_to = "study", values_to = "percent") %>% 
  mutate(Study = case_when(
    study == "this_study" ~ "this study",
    study == "blanchet_2011" ~ "Blanchet (2011)"
  )) %>% 
  ggplot(aes()) +
  geom_col(aes(mpaa, percent, 
               col = NULL,
               fill = Study),
           position = position_dodge(width = 0.4), width = 0.3) +
  scale_fill_manual(values = c("#343a40", "#57cbcc")) +
  theme_minimal() +
  labs(x = "MPAA rating", 
       y = "% from total number of films in the respective study") +
  ylim(c(0, 50))