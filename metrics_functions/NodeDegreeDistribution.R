# DISTRIBUTION OF NODE DEGREES IN A NETWORK
# formatting the database:
# columns should be named "from" and "to"

# DegreeDistribution() function calculates the degree distribution of nodes in a network.
# Parameters:
#   - data: Input dataframe containing node pairs.
# Returns:
#   - A dataframe with unique nodes and their corresponding degrees.

DegreeDistribution <- function(data) {
  
  # concatenate pairs into a new dataframe and remove duplicates in node pairs
  unique_pairs <- SortColumns(data)
  
  # initialize a vector to store counts
  all_nodes <- c(unique_pairs[,1], unique_pairs[,2])
  unique_nodes <- unique(all_nodes)
  counts <- numeric(length(unique_nodes))
  
  # loop through nodes
  for (i in seq_along(unique_nodes)) {
    node <- unique_nodes[i]
    # count the number of associated genes in both columns
    count <- sum(unique_pairs[,1]==node) + sum(unique_pairs[,2]==node)
    counts[i] <- count
  }
  
  # create a dataframe with the results
  result <- data.frame(node=unique_nodes, degree=counts)
  
  # remove duplicates
  result <- result[!duplicated(result$node), ]
  
  # return the results
  return(result)
  
}



# COMPARISON OF NODE DEGREE DISTRIBUTION BETWEEN TWO NETWORKS

# DegreeDistribution_Comparison() function compares the degree distributions of nodes in two networks and visualizes them using a bar plot.
# Parameters:
#   - data1: Input dataframe containing node pairs for the first network.
#   - data2: Input dataframe containing node pairs for the second network.
# Returns:
#   - A ggplot object visualizing the comparison of degree distributions between the two networks.

DegreeDistribution_Comparison <- function(data1, data2){
  
  # load the necessary library for the plot
  library(ggplot2)
  
  # calculate degree counts for each database
  degree_distribution_1 <- DegreeDistribution(data1)
  degree_distribution_2 <- DegreeDistribution(data2)
  
  # create dataframe for creating comparison plot
  df <- rbind(data.frame(degree_distribution_1, dataset = deparse(substitute(data1))),
              data.frame(degree_distribution_2, dataset = deparse(substitute(data2))))
  
  # create histogram plot with ggplot
  ggplot(df, aes(x=degree, y=after_stat(count), fill=dataset)) +
    # display a histogram
    geom_histogram(position="dodge", alpha=0.5) +
    # customize legends
    labs(x="Degree", 
         y="Number of Nodes", 
         title="Comparison of Degree Distributions",
         fill=expression(underline("Dataset"))) +
    # specify legend display
    guides(color="none") +
    # customize text elements
    theme(strip.text.x=element_text(size=8),
          plot.title=element_text(size=12, hjust=0.5, face="bold"),
          legend.title=element_text(size=10))
  
}

# EXAMPLE OF USE
DegreeDistribution_Comparison(DIANE, TDCor)
