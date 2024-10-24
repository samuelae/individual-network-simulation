library(igraph)
library(tidyverse)

source("02_Code/Functions/Network/edge_weight_threshold.R")
source("02_Code/Functions/Inference/infer_assonet.R")
source("02_Code/Functions/Inference/infer_rjnet.R")
source("02_Code/Functions/Network/analyze_network.R")

indy_net <- readRDS("00_Cold_Storage/04_Individual_Networks/igraph/indy_net_param_01_subj_01.rds")
behav_data <- readRDS("00_Cold_Storage/05_Behavioral_Data/data_param_01_subj_01_study_14.rds")
# behav_data <- readRDS("00_Cold_Storage/05_Behavioral_Data/data_param_01_subj_01_study_11.rds")
# behav_data <- readRDS("00_Cold_Storage/05_Behavioral_Data/data_param_01_subj_01_study_20.rds")
# behav_data <- readRDS("00_Cold_Storage/05_Behavioral_Data/data_param_01_subj_01_study_23.rds")
cue_set <- 1

# Run nw-inference -------------------------------------------------------------

this_behav_data <- behav_data[[cue_set]]

# store order of cues for later sorting
order_of_cues <- this_behav_data$fa_data$cue %>% unique()

# generate individual net subgraph and its ordered adjacency matrix
indy_net_sub <- induced_subgraph(indy_net,
                                 vids = V(indy_net)[names(V(indy_net)) %in% order_of_cues],
                                 impl = "auto")
indy_adj_mat <- as_adjacency_matrix(indy_net_sub,
                                    type = "both",
                                    attr = "weight") %>% as.matrix()
indy_adj_mat <- indy_adj_mat[order_of_cues, order_of_cues]

# generate inferred association network and ordered adjacency matrix
fa_adj_mat <- infer_assonet(this_behav_data$fa_data, ppmi = TRUE, svd = FALSE)
fa_adj_mat <- fa_adj_mat[order_of_cues, order_of_cues]
fa_graph <- graph_from_adjacency_matrix(fa_adj_mat, 
                                        weighted = TRUE, 
                                        mode = "undirected",
                                        diag = FALSE)

# generate inferred relatedness judgment network and ordered adjacency matrix
# uses workaround for imperfect rj_data that does not include all words of the cueset
rj_adj_mat <- infer_rjnet(this_behav_data$rj_data, cueset = rownames(fa_adj_mat))
rj_adj_mat <- rj_adj_mat[order_of_cues, order_of_cues]
rj_graph <- graph_from_adjacency_matrix(rj_adj_mat, 
                                        weighted = TRUE, 
                                        mode = "undirected",
                                        diag = FALSE)

# relatedness values (edge weights)
indy_rel_values <- indy_adj_mat[lower.tri(indy_adj_mat)]
fa_rel_values <- fa_adj_mat[lower.tri(fa_adj_mat)]
fa_pwr_rel_values <- fa_rel_values^0.33
rj_rel_values <- rj_adj_mat[lower.tri(rj_adj_mat)]

fa_pwr_graph <- graph_from_adjacency_matrix(fa_adj_mat^0.33, 
                                            weighted = TRUE, 
                                            mode = "undirected",
                                            diag = FALSE)

# importance values (vertex strengths)
indy_imp_values <- strength(indy_net_sub)[order_of_cues]
fa_imp_values <- strength(fa_graph)[order_of_cues]
fa_pwr_imp_values <- strength(fa_pwr_graph)[order_of_cues]
rj_imp_values <- strength(rj_graph)[order_of_cues]

cor(indy_imp_values, fa_imp_values, method = "spearman", use = "pairwise.complete.obs")
cor(indy_imp_values, fa_pwr_imp_values, method = "spearman", use = "pairwise.complete.obs")


# analyze reasons for big difference in mean strength --------------------------

# indy and rj networks have about double the number of edges
indy_net_sub
fa_graph
rj_graph


# test if log transformation helps
fa_pwr_graph <- fa_graph
edge_attr(fa_pwr_graph, "weight") <- edge_attr(fa_pwr_graph, "weight")^0.33

# node strengths are much lower in indy and rj (~4-19) than fa (~0.05-1.01)
hist(indy_imp_values, breaks = 100)
hist(fa_imp_values, breaks = 100)
hist(rj_imp_values, breaks = 30)

# edge weights
edge_attr(indy_net_sub, "weight") %>% hist(breaks = 100)
edge_attr(fa_graph, "weight") %>% hist(breaks = 100)
edge_attr(fa_pwr_graph, "weight") %>% hist()
edge_attr(rj_graph, "weight") %>% hist()

edge_attr(indy_net_sub, "weight") %>% mean()
edge_attr(fa_graph, "weight") %>% mean()
edge_attr(fa_pwr_graph, "weight") %>% mean()
edge_attr(rj_graph, "weight") %>% mean()

# edge weight correlation (comparisons for edge_weight recovery are only made if x and y are != 0):
# fa seems to have flooring effect working against it (cor = 0.11), remember, most weights are < 0.05
plot(x = indy_rel_values,
     y = fa_rel_values)
plot(x = indy_rel_values,
     y = fa_pwr_rel_values)
plot(x = indy_rel_values,
     y = rj_rel_values)

# strengths corelation: eventhough fa values are much lower, there is an ok correlation to indy values
# given the low edge weights, its actually surprising how well means-strength variance is recovered
plot(x = indy_imp_values, 
     y = fa_imp_values)
plot(x = indy_imp_values, 
     y = fa_pwr_imp_values)
plot(x = indy_imp_values, 
     y = rj_imp_values)

# manual FA inference ----------------------------------------------------------

asso <- this_behav_data$fa_data

# create count matrix
cues = asso$cue %>% unique()
resp = asso %>% select(starts_with("resp")) %>% unlist() %>% unique()
counts = matrix(0,
                nrow = length(cues),
                ncol = length(resp),
                dimnames = list(cues, resp))
dim(counts)

# fill count matrix
for(i in 1:3) {
  tmp = asso %>%
    group_by(cue, across(all_of(paste0("resp_",i)))) %>%
    summarize(n = n()) %>%
    as.matrix() %>%
    suppressMessages()
  counts[tmp[, 1:2]] = counts[tmp[, 1:2]] + as.numeric(tmp[, 3])
}

counts[1:10, 1:10]

# transform counts to ppmi
counts = counts / sum(counts)
counts = counts / (rowSums(counts) %*% t(colSums(counts)))
counts[counts < 0] = 0

counts[1:10, 1:10]

# do cosine
norm = sqrt(rowSums(counts ** 2))
cos = counts %*% t(counts) / (norm %*% t(norm))

cos[1:10, 1:10]

# remove negative and NaN values
cos[(cos < 0) | is.nan(cos)] <- 0

cos[1:10, 1:10]

# Visualize --------------------------------------------------------------------

# Plotting the matrix
image(z = indy_adj_mat, main = "Indy Adjacency Matrix", col = gray.colors(n = 256))
image(z = cos, main = "FA Adjacency Matrix", col = gray.colors(n = 256))
image(z = rj_adj_mat, main = "RJ Adjacency Matrix", col = gray.colors(n = 256))


# set fa diag to 0 (as is also done by igraph when importing into igraph)
diag(fa_adj_mat) <- 0

# Convert the matrix to a data frame for ggplot
adj_indy <- reshape2::melt(indy_adj_mat)
adj_fa <- reshape2::melt(fa_adj_mat)
adj_fa_pwr <- reshape2::melt(fa_adj_mat^0.33)
adj_rj <- reshape2::melt(rj_adj_mat)

p_data <- adj_indy %>% mutate(nw = "Ground truth") %>% 
  bind_rows(adj_fa %>% mutate(nw = "Free association")) %>% 
  bind_rows(adj_fa_pwr %>% mutate(nw = "Free association (pwr)")) %>% 
  bind_rows(adj_rj %>% mutate(nw = "Relatedness judgment"))

# Plotting with ggplot
ggplot(p_data, aes(x = Var2, y = Var1, fill = value)) + 
  geom_tile() +
  # scale_fill_gradient(low = "black", high = "white", name = "Edge weight") + 
  scale_fill_gradient(low = "white", high = "black", name = "Edge weight") + 
  labs(title = "Adjacency matrices show low edge weights of free association inference",
       subtitle = "Mixed cue set of 100 cues, assessed with 30 responses per cue") +
  theme_minimal() +
  facet_wrap(~nw) +
  theme(axis.text = element_text(size = 3),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_blank())

ggsave("03_Plots/adjacenca_matrix_comparison.png", bg = "white",
       height = 25, width = 25, units = "cm", dpi = 600)


# test sensitivity of ppmi + cosine inference ---------------------------------- 

x <- matrix(c(1, 0, 0, 0,
              0, 1, 0, 0,
              2, 0, 2, 0,
              0, 0, 1, 1), 
            nrow = 4,
            byrow = TRUE)

x <- x * 2 # virtually no difference
x <- x + 2 # huge difference

# transform counts to ppmi
x = x / sum(x)
x = x / (rowSums(x) %*% t(colSums(x)))
x[x < 0] = 0

norm = sqrt(rowSums(x ** 2))
cos = x %*% t(x) / (norm %*% t(norm))
cos[(cos < 0) | is.nan(cos)] <- 0

cos
cos[lower.tri(cos)] %>% mean()
cos[lower.tri(cos)]^0.33 %>% mean()

