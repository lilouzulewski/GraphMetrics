# SORT NODE PAIRS IN A DATABASE
# formatting the databases:
# columns should be named "from" and "to"

# SortColumns() function sorts the pairs of nodes in a dataframe and removes duplicate pairs.
# Parameters:
#   - data: Input dataframe containing gene pairs with columns 'from' and 'to'.
# Returns:
#   - A dataframe with sorted and unique gene pairs.

SortColumns <- function(data) {
  
  # create a new dataframe with sorted gene pairs
  data_sorted <- data.frame(from = pmin(data$from, data$to), to = pmax(data$from, data$to))
  
  # remove duplicate rows
  data_sorted <- unique(data_sorted)
  
  # return the sorted dataframe
  return(data_sorted)
  
}

# EXAMPLE OF USE
SortColumns(DIANE)



# CONVERTING DATAFRAMES TO GRAPHS
# formatting the databases:
# columns should be named "from" and "to"

# DataframeToGraph() function converts a dataframe of node pairs into a graph object.
# Parameters:
#   - data: Input dataframe containing node pairs with columns 'from' and 'to'.
# Returns:
#   - An igraph graph object representing the network of node pairs.

DataframeToGraph <- function(data){

  # sort gene pairs in both datasets
  data <- SortColumns(data)
  
  # concatenate gene pairs into a new dataframe for each dataset
  pairs <- cbind(from = data$from, to = data$to, pairs = paste(data$from, data$to, sep = "_"))
  
  # remove duplicates in pairs
  unique_pairs <- unique(pairs)
  
  # load required libraries
  library(tidyr)
  library(igraph)
  
  # create graph object from dataframe
  graph <- graph_from_data_frame(unique_pairs, directed=FALSE)
  
  # return the dataframe with the results
  return(graph)

}

# EXAMPLE OF USE
DataframeToGraph(DIANE)