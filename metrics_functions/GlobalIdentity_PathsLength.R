# PERCENTAGE IDENTITY FOR PATHS OF A GIVEN LENGTH

# GlobalIdentity_Length() function calculates the identity percentages of paths of a given length between two datasets of node pairs.
# Parameters:
#   - data1: The first dataframe containing node pairs.
#   - data2: The second dataframe containing node pairs.
#   - length: The specified length of paths to consider.
# Returns:
#   - A list with identity percentages for each dataset.

GlobalIdentity_Length <- function(data1, data2, length) {
  
  # convert dataframes to graphs
  graph_data1 <- DataframeToGraph(data1)
  graph_data2 <- DataframeToGraph(data2)
  
  # initialize variables to store paths
  paths_data1 <- c()
  paths_data2 <- c()
  
  # find all simple paths in each graph up to a given degree
  l_data1 <- unlist(lapply(V(graph_data1) , function(x) all_simple_paths(graph_data1, from=x, cutoff=length)), recursive = FALSE)
  l_data2 <- unlist(lapply(V(graph_data2) , function(x) all_simple_paths(graph_data2, from=x, cutoff=length)), recursive = FALSE)
  
  # filter paths to only include those with the specified degree
  l_data1 <- l_data1[lengths(l_data1) == length + 1]
  l_data2 <- l_data2[lengths(l_data2) == length + 1]
  
  # convert paths to character IDs
  paths_data1 <- lapply(1:length(l_data1), function(x) as_ids(l_data1[[x]]))
  paths_data2 <- lapply(1:length(l_data2), function(x) as_ids(l_data2[[x]]))
  
  # combine paths into dataframes
  paths_data1 <- do.call(rbind, paths_data1)
  paths_data2 <- do.call(rbind, paths_data2)
  
  # convert matrices to dataframes
  paths_data1 <- as.data.frame(paths_data1)
  paths_data2 <- as.data.frame(paths_data2)
  
  # combine paths and create a new column by uniting the paths
  paths_data1 <- data.frame(c(paths_data1, unite(paths_data1, paths, sep="_")))
  paths_data2 <- data.frame(c(paths_data2, unite(paths_data2, paths, sep="_")))
  
  # identify different gene pairs in each dataset
  different_pairs_data1 <- setdiff(paths_data2[,length+2], paths_data1[,length+2])
  different_pairs_data2 <- setdiff(paths_data1[,length+2], paths_data2[,length+2])
  
  # calculate the number of identical gene pairs in each dataset
  num_identical_pairs_data1 <- length(paths_data2[,length+1]) - length(different_pairs_data1)
  num_identical_pairs_data2 <- length(paths_data1[,length+1]) - length(different_pairs_data2)
  
  # calculate the percentage of identical gene pairs compared to the total number of unique pairs
  identity_percentage_ReferenceData1 <- (num_identical_pairs_data1 / length(paths_data1[,length+1])) * 100
  identity_percentage_ReferenceData2 <- (num_identical_pairs_data2 / length(paths_data2[,length+1])) * 100
  
  # construct the final report as a list
  report <- list(
    identity_percentage_ReferenceData1 = identity_percentage_ReferenceData1,
    identity_percentage_ReferenceData2 = identity_percentage_ReferenceData2
  )
  
  # return the results
  return(report)
  
}

# EXAMPLE OF USE
Global_Identity_Degree(DIANE, TDCor, 1)


# COMPARISON OF THE PERCENTAGE OF IDENTITY FOR PATHS OF A GIVEN LENGTH IN TWO GRAPHS

# GlobalIdentity_Length_Comparison() function compares the identity percentages of paths 
# of different lengths between two datasets.
# Parameters:
#   - data1: First input dataframe containing node pairs.
#   - data2: Second input dataframe containing node pairs.
#   - n: Maximum path length to consider.
# Returns:
#   - A dataframe with identity percentages for each path length and a plot comparing these percentages.

GlobalIdentity_Length_Comparison <- function(data1, data2, maxlength) {
  
  # convert dataframes to graphs
  graph_data1 <- DataframeToGraph(data1)
  graph_data2 <- DataframeToGraph(data2)
  
  # find all simple paths in each graph up to the specified degree
  l_data1 <- unlist(lapply(V(graph_data1) , function(x) all_simple_paths(graph_data1, from=x, cutoff=maxlength)), recursive = FALSE)
  l_data2 <- unlist(lapply(V(graph_data2) , function(x) all_simple_paths(graph_data2, from=x, cutoff=maxlength)), recursive = FALSE)
  
  # calculate the length of each path
  lengths_paths_data1 <- sapply(l_data1, length)
  lengths_paths_data2 <- sapply(l_data2, length)
  
  # find the maximum path length
  maxlength_data1 <- max(lengths_paths_data1)
  maxlength_data2 <- max(lengths_paths_data2)
  
  # determine the maximum degree to consider
  maxlength <- min(maxlength_data1, maxlength_data2) - 1
  
  # initialize dataframe to store identity percentages
  graph <- data.frame(identity_percentage_ReferenceData1 = numeric(maxlength),
                      identity_percentage_ReferenceData2 = numeric(maxlength))
  
  # loop over degrees and calculate identity percentages
  for (i in 1:maxlength){
    # calculate identity percentages only if they are not equal to 0
    if (GlobalIdentity_Length(data1,data2,i)$identity_percentage_ReferenceData1 != 0 ||
        GlobalIdentity_Length(data1,data2,i)$identity_percentage_ReferenceData2 != 0) {
      graph$identity_percentage_ReferenceData1[i] <- GlobalIdentity_Length(data1,data2,i)$identity_percentage_ReferenceData1
      graph$identity_percentage_ReferenceData2[i] <- GlobalIdentity_Length(data1,data2,i)$identity_percentage_ReferenceData2
    }
  }
  
  # create a dataframe for the graph
  df <- data.frame(length = seq(1,maxlength),
                   percentages = c(graph$identity_percentage_ReferenceData1,
                                   graph$identity_percentage_ReferenceData2),
                   reference = c(rep(deparse(substitute(data1)), length(graph$identity_percentage_ReferenceData1)),
                                 rep(deparse(substitute(data2)), length(graph$identity_percentage_ReferenceData2))))
  
  # create the plot
  library(ggplot2)
  plot <- ggplot(df, aes(x = length, y = as.numeric(percentages), color = reference)) +
    # display a scatterplot
    geom_point() +
    # customize legends
    labs(x = "Length of Paths", 
         y = "Identity Percentage", 
         title = "Length of Paths vs Identity Percentages",
         color = expression(underline("Reference"))) +
    # customize scales
    scale_x_continuous(breaks=c(seq(1, maxlength, by = 1))) +
    # customize text elements
    theme(strip.text.x = element_text(size = 8),
          plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
          legend.title = element_text(size = 10))
  
  # print the plot
  print(plot)
  
  # return the dataframe with the results
  return(graph)

}

# EXAMPLE OF USE
Degree_Identity_Comparison(DIANE,TDCor,3)
