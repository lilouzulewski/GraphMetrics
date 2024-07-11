# PERCENTAGE OF IDENTITY BETWEEN TWO GRAPHS AS A FUNCTION OF NODE DEGREE
# formatting the databases:
# columns should be named "from" and "to"

# GlobalIdentity_Degree() function calculates and compares the degree identity percentages between two datasets of node pairs.
# Parameters:
#   - data1: The first dataframe containing node pairs.
#   - data2: The second dataframe containing node pairs.
# Returns:
#   - A dataframe with degrees and their corresponding identity percentages for each dataset.
#   - A scatter plot displaying the degree versus identity percentages.

GlobalIdentity_Degree <- function(data1, data2){
  
  # determine the maximum degree
  n <- max(DegreeDistribution(data1)$degree, DegreeDistribution(data2)$degree)
  
  # create a dataframe to store identity percentages
  identity_percentages <- data.frame(degree = 1:n,
                                     ReferenceData1 = numeric(n),
                                     ReferenceData2 = numeric(n))
  # initialize columns for data references
  identity_percentages$ReferenceData1 <- 1
  identity_percentages$ReferenceData2 <- 1
  
  # iterate over each degree
  for (i in 1:n){
    # extract genes lists for each dataset based on degree
    nodes_list_data1 <- DegreeDistribution(data1)$node[DegreeDistribution(data1)$degree == i]
    nodes_list_data2 <- DegreeDistribution(data2)$node[DegreeDistribution(data2)$degree == i]
    
    # initialize dataframes to store gene pairs
    all_node_pairs_data1 <- data.frame(from = character(), to = character())
    all_node_pairs_data2 <- data.frame(from = character(), to = character())
    
    # iterate over genes in each list
    for (node in nodes_list_data1) {
      # select rows where the gene is in the "from" or "to" column
      node_pairs_data1 <- data1[data1$from == node | data1$to == node, c("from", "to")]
      # append gene pairs to all_gene_pairs_data1
      all_node_pairs_data1 <- rbind(all_node_pairs_data1, node_pairs_data1)
    }
    for (node in nodes_list_data2) {
      node_pairs_data2 <- data2[data2$from == node | data2$to == node, c("from", "to")]
      all_node_pairs_data2 <- rbind(all_node_pairs_data2, node_pairs_data2)
    }
    
    # remove duplicate gene pairs
    all_node_pairs_data1 <- unique(all_node_pairs_data1)
    all_node_pairs_data2 <- unique(all_node_pairs_data2)
    
    # calculate identity percentages and store in the dataframe
    identity_percentages$ReferenceData1[i] <- GlobalIdentity(all_node_pairs_data1, all_node_pairs_data2)$identity_percentage_ReferenceData1
    identity_percentages$ReferenceData2[i] <- GlobalIdentity(all_node_pairs_data1, all_node_pairs_data2)$identity_percentage_ReferenceData2
  }
  
  # create a dataframe for the graph
  graph <- data.frame(degree = identity_percentages$degree,
                      percentages = c(identity_percentages$ReferenceData1,
                                      identity_percentages$ReferenceData2),
                      reference = c(rep(deparse(substitute(data1)), length(identity_percentages$ReferenceData1)),
                                    rep(deparse(substitute(data2)), length(identity_percentages$ReferenceData2))),
                      zero_equal = c(identity_percentages$ReferenceData1 == 0 & identity_percentages$ReferenceData2 == 0))
  
  # remove rows with "NO NODE" or "/" in percentages
  graph$percentages[graph$percentages %in% c("NO NODE", "/")] <- NA
  
  # remove rows with NA percentages
  graph <- graph[!is.na(graph$percentages), ]
  
  # assign color based on reference or absence
  graph$color <- ifelse(graph$percentages == 0 & graph$zero_equal, "Both null values", as.character(graph$reference))
  
  # create the plot
  library(ggplot2)
  plot <- ggplot(graph, aes(x = degree, y = as.numeric(percentages), color = color)) +
    # display a scatterplot
    geom_point() +
    # customize legends
    labs(x = "Degree", 
         y = "Identity Percentage", 
         title = "Degree of nodes VS Identity percentages",
         color = expression(underline("Reference"))) +
    # specify legend display
    guides(fill = "none") +
    # customize text elements
    theme(strip.text.x = element_text(size = 8),
          plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
          legend.title = element_text(size = 10))
  
  # display the plot
  print(plot)
  
  # return the dataframe with the results
  return(identity_percentages)
  
}

# EXAMPLE OF USE
Degree_VS_IdentityPercentage(DIANE, TDCor)