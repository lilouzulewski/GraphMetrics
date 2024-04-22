# SORT GENE PAIRS IN A DATAFRAME
# formatting the databases:
# columns should be named "from" and "to"

Sort_Columns <- function(data) {
  
  # create a new dataframe with sorted gene pairs
  data_sorted <- data.frame(from = pmin(data$from, data$to), to = pmax(data$from, data$to))
  
  # remove duplicate rows
  data_sorted <- unique(data_sorted)
  
  # return the sorted dataframe
  return(data_sorted)
  
}


# CONVERTING DATAFRAMES TO GRAPHS
# formatting the databases:
# columns should be named "from" and "to"

Dataframe_To_Graph <- function(data){

  # sort gene pairs in both datasets
  data <- Sort_Columns(data)
  
  # concatenate gene pairs into a new dataframe for each dataset
  gene_pairs <- cbind(from = data$from, to = data$to, pairs = paste(data$from, data$to, sep = "_"))
  
  # remove duplicates in gene pairs
  unique_pairs <- unique(gene_pairs)
  
  # load required libraries
  library(tidyr)
  library(igraph)
  
  # create graph object from dataframe
  graph <- graph_from_data_frame(unique_pairs, directed=FALSE)
  
  # return the dataframe with the results
  return(graph)

}