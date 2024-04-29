# DISTRIBUTION OF NODE DEGREES FROM A DATABASE
# formatting the database:
# columns should be named "from" and "to"

Degree_Distribution <- function(data) {
  
  # concatenate gene pairs into a new dataframe and remove duplicates in gene pairs
  unique_pairs <- Sort_Columns(data)
  
  # initialize a vector to store counts
  all_genes <- c(unique_pairs[,1], unique_pairs[,2])
  unique_genes <- unique(all_genes)
  counts <- numeric(length(unique_genes))
  
  # loop through genes
  for (i in seq_along(unique_genes)) {
    gene <- unique_genes[i]
    # count the number of associated genes in both columns
    count <- sum(unique_pairs[,1]==gene) + sum(unique_pairs[,2]==gene)
    counts[i] <- count
  }
  
  # create a dataframe with the results
  result <- data.frame(gene=unique_genes, degree=counts)
  
  # remove duplicates
  result <- result[!duplicated(result$gene), ]
  
  # return the results
  return(result)
  
}

# DISPLAYING COMPARISON OF NODE DISTRIBUTIONS FROM TWO DATABASES

Degree_Distribution_Comparison <- function(data1, data2){
  
  # load the necessary library for the plot
  library(ggplot2)
  
  # calculate degree counts for each database
  degree_distribution_1 <- Degree_Distribution(data1)
  degree_distribution_2 <- Degree_Distribution(data2)
  
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

# SOME EXAMPLES OF USE
Degree_Distribution_Comparison(LRP6, TDCor)
Degree_Distribution_Comparison(LRP18, TDCor)
Degree_Distribution_Comparison(LRP33, TDCor)
Degree_Distribution_Comparison(LRP54, TDCor)
