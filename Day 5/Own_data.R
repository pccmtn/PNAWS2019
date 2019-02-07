##############################                   PNAWS 2019           ###########################
##############################                     DAY 5              ###########################
##############################                WORK ON OWN DATA        ###########################


setwd("~/Dropbox (RSM)/Projects/Meetup_Causal_Study/Data/")

library(data.table)
library(dplyr)
library(RTextTools)
library(qgraph)
library(quanteda)
library(RColorBrewer)
library(ggplot2)
source("~/Dropbox (RSM)/PNWC2019/PNAWS2019/Day 4/networkAnalysisAssignement.R")
source('http://sachaepskamp.com/files/NA2014/program.txt')


##############################                LOAD OWN DATA        ###########################

MEMB_all <- fread("~/Dropbox (RSM)/Projects/Meetup_Causal_Study/Data/Automated_small_sample/Members/Wave 2019-01-26 (full data)/20190127_124540_members_profiles.csv", 
                  stringsAsFactors = F) %>%
  distinct

GRPS_all <- fread("~/Dropbox (RSM)/Projects/Meetup_Causal_Study/Data/Automated_small_sample/Groups/Wave 2019-01-26 (Full)/Groups_10zip_10categories.csv", stringsAsFactors = F) %>% 
  distinct

which(MEMB_all$group_profile.group.urlname %in% unique(GRPS_all$urlname))

MEMB_all <- MEMB_all %>% filter(group_profile.group.urlname %in% unique(GRPS_all$urlname))

# MEMB_all <- fread("Day 5/members_groups_NYC.csv", stringsAsFactors = F)
# GRPS_all <- fread("Day 5/groups_NYC.csv", stringsAsFactors = F)


######## Visualize groups and how many members they have in common.  #####

MEMB_GRPS <- MEMB_all %>%
  dplyr::select(id, name, group_profile.group.urlname) %>%
  dplyr::rename(urlname = group_profile.group.urlname) %>%
  distinct %>%
  inner_join(., GRPS_all %>% dplyr::select(urlname, city) %>% distinct)

rm(MEMB_all)

members_network.m <- MEMB_GRPS %>%
  dplyr::select(id, urlname)%>%
  data.table::melt(., id.vars = c("urlname"))

n_members_per_group <- MEMB_GRPS %>%
  group_by(urlname) %>%
  summarise(n_members = n_distinct(id)) %>%
  arrange(desc(n_members)) %>%
  distinct

n_common_member_per_group <- members_network.m %>%
  inner_join(., GRPS_all %>% dplyr::select(urlname, members), by = "urlname") %>%
  full_join(., ., by = c("value")) %>%
  filter(urlname.x != urlname.y) %>%
  dplyr::select(value, urlname.x, urlname.y, members.x, members.y) %>%
  rename(i = urlname.x, j = urlname.y) %>%
  dplyr::select(-value) %>%
  group_by(i, j) %>%
  mutate(n = n(),
         weighted_n = n/(members.x+members.y-n)) %>%
  distinct() %>%
  dplyr::select(-c(members.x, members.y))

urlnames <- data.frame(urlname = n_common_member_per_group$i)

urlnames <- urlnames %>% inner_join(., GRPS_all %>% dplyr::select(urlname, categories.shortname)) %>% distinct


qgraph(n_common_member_per_group %>% dplyr::select(i, j, n), layout = "spring", 
       edge.color = 'darkblue', 
       vsize = 3,label.cex = 7,
       directed = F, minimum = 2,
       title = "Groups in NYC", bg = "black")

######## Visualize groups by group type  #####

adj <- members_network.m %>%
  full_join(., ., by = "value") %>%
  filter(urlname.x != urlname.y) %>%
  dplyr::select(value, urlname.x, urlname.y) %>%
  rename(i = urlname.x, j = urlname.y) %>%
  dplyr::select(-value) %>%
  inner_join(., urlnames %>% distinct %>% dplyr::rename(i = urlname))

g.1 <- adj %>%
  dplyr::select(i, j) %>%
  as.matrix %>%
  graph.edgelist(., directed = F)

Edgelist.cat <- get.adjacency(g.1)
# Labels:
V(g.1)$labels <- names(V(g.1))
Labels <- V(g.1)$labels

num_colors <- nlevels(factor(adj$categories.shortname))

color_pallete_function <- colorRampPalette(
  colors = c("red", "yellow", "blue"))

cat_colors <- color_pallete_function(num_colors)

Colors <- data.frame(categories.shortname = levels(factor(urlnames$categories.shortname)), Col = cat_colors)

grp_cols <- data.frame(urlname = colnames(Edgelist.cat)) %>% 
  inner_join(., GRPS_all %>% dplyr::select(urlname, categories.shortname)) %>% 
  distinct %>%
  inner_join(., Colors)

g <- qgraph(Edgelist.cat, labels = Labels, groups = as.character(grp_cols$categories.shortname), 
            color = as.character(grp_cols$Col),
            layout= "spring", directed = FALSE, edge.color = "blue",
            vsize = 5, esize = 2, label.cex = 5, legend = F, bg = "black", label.color = "white",
            title = "Groups by group type")


######### Members connections ###########


# I need to select some small sample. Let's just take the members of one group.

# members_group <- unique(GRPS_all$urlname[GRPS_all$urlname == sample(GRPS_all$urlname, 1)])
# 
# sample_members <- members_network.m %>%
#   filter(urlname %like% members_group)
# 
# sample_members <- unique(sample_members$value)
# 
# members_network.m_small <- members_network.m %>%
#   filter(value %in% sample_members)

members_network <- members_network.m %>%
  full_join(., ., by = "urlname") %>%
  filter(value.x != value.y) %>%
  dplyr::select(urlname, value.x, value.y) %>%
  dplyr::rename(i = value.x, j = value.y)

edge_list <- members_network %>%
  group_by(i, j) %>%
  mutate(n = n()) %>%
  ungroup %>%
  distinct %>%
  filter(n > 1)

edge_list_small <- edge_list %>%
  dplyr::rename(Source= i, Target = j, Weight = n) %>%
  as.data.frame() %>%
  distinct

g.2 <- edge_list_small %>%
  dplyr::select(Source, Target) %>%
  mutate_all(., .funs = function(x) as.character(x)) %>%
  as.matrix %>%
  graph.edgelist(., directed = F)

V(g.2)$label <- names(V(g.2))
Labels <- V(g.2)$label

Edgelist.membs <- get.adjacency(g.2)


g <- qgraph(Edgelist.membs,directed = FALSE, layout = "spring", 
            edge.color = "darkblue", label.color = "darkblue", bg = "black", border.color = "darkblue",
            vsize = 1)

# analyze how important or central nodes are in the network
Centrality <- centrality(g.2, all.shortest.paths = TRUE)

Betweenness <- as.data.frame(Centrality$Betweenness)
Betweenness$id <- rownames(Betweenness)

Betweenness$Closeness <- Centrality$Closeness

Betweenness$Node_strength <- Centrality$OutDegree


### node strength 

Betweenness <- Betweenness %>% as.data.frame() %>% arrange(desc(Node_strength))

Betweenness[1, ]

MEMB_all[MEMB_all$id == Betweenness[1, ]$id, c("name", "id", "bio")] %>% distinct

## Let's see who the strongest person is:

# Paul

Betweenness[2, ]

MEMB_all[MEMB_all$id == Betweenness[2, ]$id, c("name", "id", "bio")] %>% distinct

## Let's see who the second strongest person is:

# Charlie Stevens

### Betweenness

Betweenness <- Betweenness %>% as.data.frame() %>% arrange(desc(`Centrality$Betweenness`))

Betweenness[1, ]

MEMB_all[MEMB_all$id == Betweenness[1, ]$id, c("name", "id", "bio")] %>% distinct

## Let's see who the strongest person is:

# Taner

Betweenness[2, ]

MEMB_all[MEMB_all$id == Betweenness[2, ]$id, c("name", "id", "bio")] %>% distinct

## Let's see who the second strongest person is:

# Paul -- the same as node strength.

### Closeness

Betweenness <- Betweenness %>% as.data.frame() %>% arrange(desc(Closeness))

Betweenness[1, ]

MEMB_all[MEMB_all$id == Betweenness[1, ]$id, c("name", "id", "bio")] %>% distinct

## Let's see who the strongest person is:

# Taner, same as betweenness

Betweenness[2, ]

MEMB_all[MEMB_all$id == Betweenness[2, ]$id, c("name", "id", "bio")] %>% distinct

## Let's see who the second strongest person is:

# Paul -- the same as node strength and betweenness.

## Let's see more about this Paul:

n_groups_per_member <- MEMB_GRPS %>%
  group_by(id, name) %>%
  summarise(n_groups = n_distinct(urlname)) %>%
  inner_join(., MEMB_GRPS %>% distinct(id, name), by = c("id", "name")) %>%
  arrange(desc(n_groups)) %>%
  distinct

n_groups_per_member[n_groups_per_member$id == 31492942, ]

# He is part of 10 other groups: seems to be into enterpreneurship.

MEMB_GRPS %>% filter(id == 31492942) %>% distinct

######### Categories connections ###########

adj_categories <- members_network.m %>%
  full_join(., ., by = "variable") %>%
  filter(value.x != value.y & urlname.x != urlname.y) %>%
  inner_join(., GRPS_all %>% distinct(urlname, categories.shortname) %>% dplyr::rename(urlname.x = urlname)) %>%
  inner_join(., GRPS_all %>% distinct(urlname, categories.shortname) %>% dplyr::rename(urlname.y = urlname,
                                                                                       categories.shortname.y = categories.shortname))
adj_categories <- adj_categories %>%
  select(categories.shortname, categories.shortname.y)

edge_list_categories <- adj_categories %>%
  dplyr::rename(i = categories.shortname, j = categories.shortname.y) %>%
  group_by(i, j) %>%
  mutate(n = n()) %>%
  ungroup %>%
  distinct

g.3 <- edge_list_categories %>%
  dplyr::rename(Source = i, Target = j) %>%
  dplyr::select(Source, Target) %>%
  mutate_all(., .funs = function(x) as.character(x)) %>%
  as.matrix %>%
  graph.edgelist(., directed = F)

V(g.2)$label <- names(V(g.2))
Labels <- V(g.2)$label

Edgelist.cats <- get.adjacency(g.3)


g3 <- qgraph(Edgelist.cats,directed = FALSE, layout = "spring", edge.color = "black",
            vsize = 5)

