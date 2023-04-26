# Lab 1b: Descriptive Network Analysis

# Start with a clear environment
rm(list=ls())

######################################################################################
list.of.packages <- c("robustbase","igraph","statnet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(igraph)

# To check whether your R loads these packages, run the following code
sessionInfo() 

# Load the dataset 
load('/Users/jianingjin/Desktop/IEMS341/Lab_1a.RData')

# Take out the largest component from the graph
gpt_comp <- components(gpt_graph)
giantGraph_gpt <- gpt_graph %>% 
  induced.subgraph(., which(gpt_comp$membership == which.max(gpt_comp$csize)))
vcount(gpt_graph) ## the number of nodes/actors/users
ecount(gpt_graph) ## the number of edges

# now repeat steps with the collective intelligence network
hmn_comp <- components(hmn_graph)
giantGraph_hmn <- hmn_graph %>% 
  induced.subgraph(., which(hmn_comp$membership == which.max(hmn_comp$csize)))
vcount(hmn_graph) ## the number of nodes/actors/users
ecount(hmn_graph) ## the number of edges

######################################################################################
#
# Part III: Local Network Properties
#
######################################################################################

# create a 'sna' graph object from an 'igraph' object
sna_gpt <- igraph::get.adjacency(giantGraph_gpt, sparse=FALSE) %>% network::as.network.matrix()
sna_hmn <- igraph::get.adjacency(giantGraph_hmn, sparse=FALSE) %>% network::as.network.matrix()

detach('package:igraph')
library(statnet)

# Compute centralities based on 'network' package
# Calculate in-degree centrality
idegScores_gpt <- degree(sna_gpt, cmode = 'indegree')
idegScores_hmn <- degree(sna_hmn, cmode = 'indegree')
# Store the information
centralities_gpt <- data.frame('node_name' = as.character(network.vertex.names(sna_gpt)),
                               'in_degree' = degree(sna_gpt, cmode = 'indegree'))
centralities_hmn <- data.frame('node_name' = as.character(network.vertex.names(sna_hmn)),
                               'in_degree' = degree(sna_hmn, cmode = 'indegree'))
# Calculate out-degree centrality and store it in the data.frame called 'centralities'
centralities_gpt$out_degree <- degree(sna_gpt, cmode = 'outdegree')
centralities_hmn$out_degree <- degree(sna_hmn, cmode = 'outdegree')

# Calculate betweenness centrality and store it in the data.frame called 'centralities'
centralities_gpt$betweenness <- betweenness(sna_gpt)
centralities_hmn$betweenness <- betweenness(sna_hmn)

# Calculate closeness centrality and store it in the data.frame called 'centralities'
centralities_gpt$incloseness <- igraph::closeness(giantGraph_gpt, mode = 'in')
centralities_gpt$outcloseness <- igraph::closeness(giantGraph_gpt, mode = 'out')

centralities_hmn$incloseness <- igraph::closeness(giantGraph_hmn, mode = 'in')
centralities_hmn$outcloseness <- igraph::closeness(giantGraph_hmn, mode = 'out')

# Calculate eigenvector centrality and store it in the data.frame called 'centralities'
# using 'igraph' because the code implemented in 'sna' is unreliable
centralities_gpt$eigen <- igraph::eigen_centrality(giantGraph_gpt)$vector
centralities_hmn$eigen <- igraph::eigen_centrality(giantGraph_hmn)$vector

# Calculate Burt's network constraint and store it in the data.frame called 'centralities'
# using 'igraph' because 'sna' doesn't have the function
centralities_gpt$netconstraint <- igraph::constraint(giantGraph_gpt)
centralities_hmn$netconstraint <- igraph::constraint(giantGraph_hmn)
#help(constraint) 
# High constraint = redundant contacts, low constraint = acting as a broker

# Calculate authority and store it in the data.frame called 'centralities'
# using 'igraph' because 'sna' doesn't have the function
# 'igraph::' allows calling for any igraph function without loading the package
centralities_gpt$authority <- igraph::authority_score(giantGraph_gpt, scale = TRUE)$`vector`
centralities_hmn$authority <- igraph::authority_score(giantGraph_hmn, scale = TRUE)$`vector`

# Calculate hub and store it in the data.frame called 'centralities'
# using 'igraph' because 'sna' doesn't have the function
centralities_gpt$hub <- igraph::hub_score(giantGraph_gpt, scale = TRUE)$`vector`
centralities_hmn$hub <- igraph::hub_score(giantGraph_hmn, scale = TRUE)$`vector`

View(centralities_gpt)
View(centralities_hmn)

######################################################################################
#
# Part IV: Global Network Properties
#
######################################################################################
# before recalling 'igraph'
detach('package:statnet', unload = TRUE)
library(igraph)

## calculate k-cores
kcore_gpt <- giantGraph_gpt %>% graph.coreness(.)
kcore_gpt ## show the results of k-core decomposition
kcore_hmn <- giantGraph_hmn %>% graph.coreness(.)
kcore_hmn ## show the results of k-core decomposition

## Plot a graph colored by the k-core decomposition results
giantGraph_gpt %>% 
  plot(.,
       layout = layout_with_gem(.),
       edge.arrow.size = .3,
       vertex.size = 4,
       vertex.label = NA,
       vertex.color = adjustcolor(graph.coreness(.), alpha.f = .3),
       vertex.label.cex = .5,
       vertex.label.color = 'black',
       mark.groups = by(seq_along(graph.coreness(.)), graph.coreness(.), invisible),
       mark.shape = 1/4,
       mark.col = rainbow(length(unique(graph.coreness(.))),alpha = .1),
       mark.border = NA
  )

giantGraph_hmn %>% 
  plot(.,
       layout = layout_with_gem(.),
       edge.arrow.size = .3,
       vertex.size = 4,
       vertex.label = NA,
       vertex.color = adjustcolor(graph.coreness(.), alpha.f = .3),
       vertex.label.cex = .5,
       vertex.label.color = 'black',
       mark.groups = by(seq_along(graph.coreness(.)), graph.coreness(.), invisible),
       mark.shape = 1/4,
       mark.col = rainbow(length(unique(graph.coreness(.))),alpha = .1),
       mark.border = NA
  )

# Plot the number of clusters in the graph and their size
cluster_gpt <- giantGraph_gpt %>% cluster_edge_betweenness() 
cluster_hmn <- giantGraph_hmn %>% cluster_edge_betweenness() 

cluster_gpt
cluster_hmn

# modularity measure
modularity(cluster_gpt)
modularity(cluster_hmn)

# Find the number of clusters
membership(cluster_gpt)   # affiliation list
membership(cluster_hmn)   # affiliation list
length(cluster_gpt) # number of clusters
length(cluster_hmn) # number of clusters

# Find the size the each cluster 
# Note that communities with one node are isolates, or have only a single tie
sizes(cluster_gpt) 
sizes(cluster_hmn) 

# Visualize clusters 
cluster_gpt %>% plot(.,giantGraph_gpt,
                 # layout = layout_with_gem(.),
                 layout = layout_with_fr(giantGraph_gpt),
                 edge.arrow.size = .3,
                 vertex.size = 4,
                 vertex.label = NA,
                 vertex.color = adjustcolor(membership(.), alpha.f = .3),
                 vertex.label.cex = .5,
                 vertex.label.color = 'black',
                 mark.groups = by(seq_along(membership(.)), membership(.), invisible),
                 mark.shape = 1/4,
                 mark.col = rainbow(length(.),alpha = .1),
                 mark.border = NA
)

cluster_hmn %>% plot(.,giantGraph_hmn,
                 # layout = layout_with_gem(.),
                 layout = layout_with_fr(giantGraph_hmn),
                 edge.arrow.size = .3,
                 vertex.size = 4,
                 vertex.label = NA,
                 vertex.color = adjustcolor(membership(.), alpha.f = .3),
                 vertex.label.cex = .5,
                 vertex.label.color = 'black',
                 mark.groups = by(seq_along(membership(.)), membership(.), invisible),
                 mark.shape = 1/4,
                 mark.col = rainbow(length(.),alpha = .1),
                 mark.border = NA
)


# Examine the in-degree distribution
giantGraph_gpt %>% degree.distribution(.,mode="in") %>% 
  plot(., col = 'black', pch = 19, cex = 1.5,
       main = 'ChatGPT In-degree Distribution',
       ylab = 'Density',
       xlab = 'In-degree')
giantGraph_hmn %>% degree.distribution(.,mode="in") %>% 
  plot(., col = 'black', pch = 19, cex = 1.5,
       main = 'WikiPedia In-degree Distribution',
       ylab = 'Density',
       xlab = 'In-degree')

# CCDF - Complementary Cumulative Distribution Function
# Plot a log-log plot of in-degree distribution
giantGraph_gpt %>% 
  degree.distribution(.,cumulative = TRUE,mode ='in') %>% 
  plot(1:(max(degree(giantGraph_gpt,mode='in'))+1),., ## since log doesn't take 0, add 1 to every degree
       log='xy', type = 'l',
       main = 'ChatGPT Log-Log Plot of In-degree',
       ylab = 'CCDF',
       xlab = 'In-degree')
giantGraph_hmn %>% 
  degree.distribution(.,cumulative = TRUE,mode ='in') %>% 
  plot(1:(max(degree(giantGraph_hmn,mode='in'))+1),., ## since log doesn't take 0, add 1 to every degree
       log='xy', type = 'l',
       main = 'WikiPedia Log-Log Plot of In-degree',
       ylab = 'CCDF',
       xlab = 'In-degree')

# Examine the out-degree distribution
giantGraph_gpt %>% degree.distribution(.,mode="out") %>% 
  plot(., col = 'black', pch = 19, cex = 1.5,
       main = 'ChatGPT Out-degree Distribution',
       ylab = 'Density',
       xlab = 'Out-degree')
giantGraph_hmn %>% degree.distribution(.,mode="out") %>% 
  plot(., col = 'black', pch = 19, cex = 1.5,
       main = 'WikiPedia Out-degree Distribution',
       ylab = 'Density',
       xlab = 'Out-degree')

# Plot a log-log plot of out-degree distribution
giantGraph_gpt %>% 
  degree.distribution(.,cumulative = TRUE,mode ='out') %>% 
  plot(1:(max(degree(giantGraph_gpt,mode='out'))+1),., ## since log doesn't take 0, add 1 to every degree
       log='xy', type = 'l',
       main = 'ChatGPT Log-Log Plot of Out-degree',
       ylab = 'CCDF',
       xlab = 'Out-degree')
giantGraph_hmn %>% 
  degree.distribution(.,cumulative = TRUE,mode ='out') %>% 
  plot(1:(max(degree(giantGraph_hmn,mode='out'))+1),., ## since log doesn't take 0, add 1 to every degree
       log='xy', type = 'l',
       main = 'WikiPedia Log-Log Plot of Out-degree',
       ylab = 'CCDF',
       xlab = 'Out-degree')

# Fit a power law to the degree distribution
in_power_gpt <- giantGraph_gpt %>% 
  degree.distribution(., mode='in') %>%
  power.law.fit(., xmin=0.00000001)
in_power_gpt

in_power_hmn <- giantGraph_hmn %>% 
  degree.distribution(., mode='in') %>%
  power.law.fit(., xmin=0.00000001)
in_power_hmn

# Examine the out-degree distribution
giantGraph_gpt %>% degree.distribution(.,mode="out") %>% 
  plot(., col = 'black', pch = 19, cex = 1.5,
       main = 'Out-degree Distribution',
       ylab = 'Density',
       xlab = 'Out-degree')
giantGraph_hmn %>% degree.distribution(.,mode="out") %>% 
  plot(., col = 'black', pch = 19, cex = 1.5,
       main = 'Out-degree Distribution',
       ylab = 'Density',
       xlab = 'Out-degree')

# Plot a log-log plot
giantGraph_gpt %>% 
  degree.distribution(.,cumulative = TRUE,mode ='out') %>% 
  plot(1:(max(degree(giantGraph_gpt,mode='out'))+1), ## since log doesn't take 0, add 1 to every degree
       ., log='xy', type = 'l',
       main = 'Log-Log Plot of Out-degree',
       ylab = 'CCDF',
       xlab = 'Out-degree')
giantGraph_hmn %>% 
  degree.distribution(.,cumulative = TRUE,mode ='out') %>% 
  plot(1:(max(degree(giantGraph_hmn,mode='out'))+1), ## since log doesn't take 0, add 1 to every degree
       ., log='xy', type = 'l',
       main = 'Log-Log Plot of Out-degree',
       ylab = 'CCDF',
       xlab = 'Out-degree')
# Fit a power law to the degree distribution
out_power_gpt <- giantGraph_gpt %>% 
  degree.distribution(., mode='out') %>%
  power.law.fit(.)
out_power_gpt

out_power_hmn <- giantGraph_hmn %>% 
  degree.distribution(., mode='out')

out_power_hmn <- out_power_hmn + 0.000000001 # prevents numerical errors with negative values
  
out_power_hmn <- out_power_hmn %>% 
  power.law.fit(.)

out_power_hmn

# Small-world Characteristics
ntrials <- 1000 ## set a value for the repetition
cl.rg_gpt <- numeric(ntrials) ## create an estimated value holder for clustering coefficient
apl.rg_gpt <- numeric(ntrials) ## create an estimated value holder for average path length
for (i in (1:ntrials)) {
  g.rg <- rewire(giantGraph_gpt, keeping_degseq(niter = 100))
  cl.rg_gpt[i] <- transitivity(g.rg, type = 'average')
  apl.rg_gpt[i] <- average.path.length(g.rg)
}

cl.rg_hmn <- numeric(ntrials) ## create an estimated value holder for clustering coefficient
apl.rg_hmn <- numeric(ntrials) ## create an estimated value holder for average path length
for (i in (1:ntrials)) {
  g.rg <- rewire(giantGraph_hmn, keeping_degseq(niter = 100))
  cl.rg_hmn[i] <- transitivity(g.rg, type = 'average')
  apl.rg_hmn[i] <- average.path.length(g.rg)
}

# plot a histogram of simulated values for clustering coefficient + the observed value
hist(cl.rg_gpt,
     main = 'Histogram of Clustering Coefficient',
     xlab = 'Clustering Coefficient', xlim = c(0, 1))
par(xpd = FALSE)

# the line indicates the mean value of clustering coefficient for your network
abline(v = giantGraph_gpt %>% transitivity(., type = 'average'), col = 'red', lty = 2)

# this tests whether the observed value is statistically different from the simulated distribution
t.test(cl.rg_gpt, mu=giantGraph_gpt %>% transitivity(., type = 'average'),
       alternative = 'less') ## pick either 'less' or 'greater' based on your results

# repeat the code above on the collective intelligence network
hist(cl.rg_hmn,
     main = 'Histogram of Clustering Coefficient',
     xlab = 'Clustering Coefficient', xlim = c(0, 1))
par(xpd = FALSE)
abline(v = giantGraph_hmn %>% transitivity(., type = 'average'), col = 'red', lty = 2)
t.test(cl.rg_hmn, mu=giantGraph_hmn %>% transitivity(., type = 'average'),
       alternative = 'less')

# plot a histogram of simulated values for average path length + the observed value
hist(apl.rg_gpt,
     main = 'Histogram of Average Path Length',
     xlab = 'Average Path Length')
# the line indicates the mean value of average path length for your network
abline(v = giantGraph_gpt %>% average.path.length(), col = 'red', lty = 2)
# this tests whether the observed value is statistically different from the simulated distribution
t.test(apl.rg_gpt, mu=giantGraph_gpt %>% average.path.length(.),
       alternative = 'less') 

# repeat the code above on the collective intelligence network
hist(apl.rg_hmn,
     main = 'Histogram of Average Path Length',
     xlab = 'Average Path Length')
abline(v = giantGraph_hmn %>% average.path.length(), col = 'red', lty = 2)
t.test(apl.rg_hmn, mu=giantGraph_hmn %>% average.path.length(.),
       alternative = 'less')


# The following command saves the R environment as RData
save.image('Lab1b_Descriptive.RData')
load('Lab1b_Descriptive.RData')

