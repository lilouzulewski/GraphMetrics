# GLOBAL IDENTITY PERCENTAGE OF A GRAPH AT DEGREE n

# function to calculate global identity percentage of a graph at a given degree
Global_Identity_Degree <- function(data1, data2, degree) {
  
  # convert dataframes to graphs
  graph_data1 <- Dataframe_To_Graph(data1)
  graph_data2 <- Dataframe_To_Graph(data2)
  
  # initialize variables to store paths
  paths_data1 <- c()
  paths_data2 <- c()
  
  # find all simple paths in each graph up to a given degree
  l_data1 <- unlist(lapply(V(graph_data1) , function(x) all_simple_paths(graph_data1, from=x, cutoff=degree)), recursive = FALSE)
  l_data2 <- unlist(lapply(V(graph_data2) , function(x) all_simple_paths(graph_data2, from=x, cutoff=degree)), recursive = FALSE)
  
  # filter paths to only include those with the specified degree
  l_data1 <- l_data1[lengths(l_data1) == degree + 1]
  l_data2 <- l_data2[lengths(l_data2) == degree + 1]
  
  # convert paths to character IDs
  paths_data1 <- lapply(1:length(l_data1), function(x) as_ids(l_data1[[x]]))
  paths_data2 <- lapply(1:length(l_data2), function(x) as_ids(l_data2[[x]]))
  
  # combine paths into dataframes
  paths_data1 <- do.call(rbind, paths_data1)
  paths_data2 <- do.call(rbind, paths_data2)
  
  # combine paths and create a new column by uniting the paths
  paths_data1 <- data.frame(c(paths_data1, unite(paths_data1, paths, sep="_")))
  paths_data2 <- data.frame(c(paths_data2, unite(paths_data2, paths, sep="_")))
  
  # identify different gene pairs in each dataset
  different_pairs_data1 <- setdiff(paths_data2[,degree+2], paths_data1[,degree+2])
  different_pairs_data2 <- setdiff(paths_data1[,degree+2], paths_data2[,degree+2])
  
  # calculate the number of identical gene pairs in each dataset
  num_identical_pairs_data1 <- length(paths_data2[,degree+1]) - length(different_pairs_data1)
  num_identical_pairs_data2 <- length(paths_data1[,degree+1]) - length(different_pairs_data2)
  
  # calculate the percentage of identical gene pairs compared to the total number of unique pairs
  identity_percentage_ReferenceData1 <- (num_identical_pairs_data1 / length(paths_data1[,degree+1])) * 100
  identity_percentage_ReferenceData2 <- (num_identical_pairs_data2 / length(paths_data2[,degree+1])) * 100
  
  # construct the final report as a list
  report <- list(
    identity_percentage_ReferenceData1 = identity_percentage_ReferenceData1,
    identity_percentage_ReferenceData2 = identity_percentage_ReferenceData2
  )
  
  # return the results
  return(report)
}

# example usage of Global_Identity_Degree function
Global_Identity_Degree(LRP27, TDCor, 1)


# DEGREE IDENTITY COMPARISON

# function to compare identity percentages at different degrees
Degree_Identity_Comparison <- function(data1, data2, n) {
  
  # convert dataframes to graphs
  graph_data1 <- Dataframe_To_Graph(data1)
  graph_data2 <- Dataframe_To_Graph(data2)
  
  # find all simple paths in each graph up to the specified degree
  l_data1 <- unlist(lapply(V(graph_data1) , function(x) all_simple_paths(graph_data1, from=x, cutoff=n)), recursive = FALSE)
  l_data2 <- unlist(lapply(V(graph_data2) , function(x) all_simple_paths(graph_data2, from=x, cutoff=n)), recursive = FALSE)
  
  # calculate the length of each path
  lengths_paths_data1 <- sapply(l_data1, length)
  lengths_paths_data2 <- sapply(l_data2, length)
  
  # find the maximum path length
  longueur_max_data1 <- max(lengths_paths_data1)
  longueur_max_data2 <- max(lengths_paths_data2)
  
  # determine the maximum degree to consider
  n <- min(longueur_max_data1, longueur_max_data2) - 1
  
  # initialize dataframe to store identity percentages
  graph <- data.frame(identity_percentage_ReferenceData1 = numeric(n),
                      identity_percentage_ReferenceData2 = numeric(n))
  
  # loop over degrees and calculate identity percentages
  for (i in 1:n){
    # calculate identity percentages only if they are not equal to 0
    if (Global_Identity_Degree(data1,data2,i)$identity_percentage_ReferenceData1 != 0 ||
        Global_Identity_Degree(data1,data2,i)$identity_percentage_ReferenceData2 != 0) {
      graph$identity_percentage_ReferenceData1[i] <- Global_Identity_Degree(data1,data2,i)$identity_percentage_ReferenceData1
      graph$identity_percentage_ReferenceData2[i] <- Global_Identity_Degree(data1,data2,i)$identity_percentage_ReferenceData2
    }
  }
  
  # create a dataframe for the graph
  df <- data.frame(length = seq(1,n),
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
    scale_x_continuous(breaks=c(seq(1, n, by = 1))) +
    # customize text elements
    theme(strip.text.x = element_text(size = 8),
          plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
          legend.title = element_text(size = 10))
  
  # print the plot
  print(plot)
  
  # return the dataframe with the results
  return(graph)
}

# example usage of Degree_Identity_Comparison function
a <- Degree_Identity_Comparison(LRP27,TDCor,3)
