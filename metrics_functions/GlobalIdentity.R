# PERCENTAGE IDENTITY OF EDGES BETWEEN TWO NETWORKS
# formatting the databases:
# columns should be named "from" and "to"

# GlobalIdentity() function calculates the global identity percentage of node pairs between two datasets.
# Parameters:
#   - data1: Input dataframe containing node pairs for the first dataset.
#   - data2: Input dataframe containing node pairs for the second dataset.
# Returns:
#   - A list with the percentage of identical node pairs in each dataset compared to the other.

GlobalIdentity <- function(data1, data2) {
  
  # check if data1 or data2 is empty
  if (nrow(data1) == 0) {
    return(list(
      identity_percentage_ReferenceData1 = "NO NODE",
      identity_percentage_ReferenceData2 = "/"
    ))
  }
  if (nrow(data2) == 0) {
    return(list(
      identity_percentage_ReferenceData1 = "/",
      identity_percentage_ReferenceData2 = "NO NODE"
    ))
  }
  
  # sort gene pairs in both datasets
  data1 <- SortColumns(data1)
  data2 <- SortColumns(data2)
  
  # concatenate gene pairs into a new dataframe for each dataset
  node_pairs_data1 <- cbind(from = data1$from, to = data1$to, pairs = paste(data1$from, data1$to, sep = "_"))
  node_pairs_data2 <- cbind(from = data2$from, to = data2$to, pairs = paste(data2$from, data2$to, sep = "_"))
  
  # remove duplicates in gene pairs
  unique_pairs_data1 <- unique(node_pairs_data1)
  unique_pairs_data2 <- unique(node_pairs_data2)
  
  # identify different gene pairs in each dataset
  different_pairs_data1 <- setdiff(unique_pairs_data2[, 3], unique_pairs_data1[, 3])
  different_pairs_data2 <- setdiff(unique_pairs_data1[, 3], unique_pairs_data2[, 3])
  
  # calculate the number of identical gene pairs in each dataset
  num_identical_pairs_data1 <- length(unique_pairs_data2[, 3]) - length(different_pairs_data1)
  num_identical_pairs_data2 <- length(unique_pairs_data1[, 3]) - length(different_pairs_data2)
  
  # calculate the percentage of identical gene pairs compared to the total number of unique pairs
  identity_percentage_ReferenceData1 <- (num_identical_pairs_data1 / length(unique_pairs_data1[, 3])) * 100
  identity_percentage_ReferenceData2 <- (num_identical_pairs_data2 / length(unique_pairs_data2[, 3])) * 100
  
  # construct the final report as a list
  report <- list(
    identity_percentage_ReferenceData1 = identity_percentage_ReferenceData1,
    identity_percentage_ReferenceData2 = identity_percentage_ReferenceData2
  )
  
  # return the results
  return(report)
  
}


# EXAMPLE OF USE
GlobalIdentity(DIANE, TDCor)