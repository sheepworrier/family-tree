library(visNetwork)
library(tidyverse)
# library(data.tree)
# Load the CSV files
relationships <- read_csv("family-tree/Relationships-Table 1.csv")
people <- read_csv("family-tree/People-Table 1.csv")
edges <- read_csv("family-tree/Edges-Table 1.csv")
# Replace NAs in the relationship start dates with ???
relationships$`Start Date`[is.na(relationships$`Start Date`)] <- "???"
# Set the arrowheads to appear in the middle of the line connecting nodes
edges$arrows <- "middle"
# Refromat people and relationships into a node dataframe
ppl_nodes <- people %>%
  mutate(label = paste(Forename, "\n", Surname),
         shape = "image",
         title = paste0("<b>",
                        ifelse(is.na(`Middle Names`),
                               paste(Forename, Surname),
                               paste(Forename, `Middle Names`, Surname)),
                        "</b><p>Born on: ", DOB)) %>%
  rename(level = Level) %>%
  select(id, label, shape, title, level)
rel_nodes <- relationships %>%
  mutate(label = `Start Date`,
         shape = "image",
         title = paste0("<b>From: ", `Start Date`,
                        ifelse(is.na(`End Date`), "", paste(" To:",
                                                            `End Date`)),
                        "</b>"),
         icon.face = "Ionicons",
         icon.code = ifelse(Type == "Divorce", "heart-dislike", "heart")) %>%
  rename(level = Level) %>%
  select(id, label, shape, title, level)
nodes <- rbind(ppl_nodes, rel_nodes) %>% select(id, label, level, shape)
# Create visualisation
visNetwork(nodes, edges, width = "100%", height = "100vh") %>%
  # addIonicons() %>%
  # visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visPhysics(enabled = TRUE) %>%
  visHierarchicalLayout(sortMethod = "directed", direction = "DU")
  # visHierarchicalLayout(nodeSpacing = 175, sortMethod = "hubsize")