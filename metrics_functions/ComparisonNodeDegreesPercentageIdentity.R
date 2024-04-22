# PERCENTAGE OF IDENTITY BETWEEN TWO GRAPHS AS A FUNCTION OF NODE DEGREE
# formatting the databases:
# columns should be named "from" and "to"

Degree_VS_IdentityPercentage <- function(data1, data2){
  
  # determine the maximum degree
  n <- max(Degree_Distribution(data1)$degree, Degree_Distribution(data2)$degree)
  
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
    genes_list_data1 <- Degree_Distribution(data1)$gene[Degree_Distribution(data1)$degree == i]
    genes_list_data2 <- Degree_Distribution(data2)$gene[Degree_Distribution(data2)$degree == i]
    
    # initialize dataframes to store gene pairs
    all_gene_pairs_data1 <- data.frame(from = character(), to = character())
    all_gene_pairs_data2 <- data.frame(from = character(), to = character())
    
    # iterate over genes in each list
    for (gene in genes_list_data1) {
      # select rows where the gene is in the "from" or "to" column
      gene_pairs_data1 <- data1[data1$from == gene | data1$to == gene, c("from", "to")]
      # append gene pairs to all_gene_pairs_data1
      all_gene_pairs_data1 <- rbind(all_gene_pairs_data1, gene_pairs_data1)
    }
    for (gene in genes_list_data2) {
      gene_pairs_data2 <- data2[data2$from == gene | data2$to == gene, c("from", "to")]
      all_gene_pairs_data2 <- rbind(all_gene_pairs_data2, gene_pairs_data2)
    }
    
    # remove duplicate gene pairs
    all_gene_pairs_data1 <- unique(all_gene_pairs_data1)
    all_gene_pairs_data2 <- unique(all_gene_pairs_data2)
    
    # calculate identity percentages and store in the dataframe
    identity_percentages$ReferenceData1[i] <- Global_Identity(all_gene_pairs_data1, all_gene_pairs_data2)$identity_percentage_ReferenceData1
    identity_percentages$ReferenceData2[i] <- Global_Identity(all_gene_pairs_data1, all_gene_pairs_data2)$identity_percentage_ReferenceData2
  }
  
  # create a dataframe for the graph
  graph <- data.frame(degree = identity_percentages$degree,
                      percentages = c(identity_percentages$ReferenceData1,
                                      identity_percentages$ReferenceData2),
                      reference = c(rep(deparse(substitute(data1)), length(identity_percentages$ReferenceData1)),
                                    rep(deparse(substitute(data2)), length(identity_percentages$ReferenceData2))))
  
  # modify the values for red points and NA values to 0
  graph$percentages[graph$percentages %in% c("NO NODE", "/")] <- NA
  graph$color <- ifelse(is.na(graph$percentages), "Absent degree", as.character(graph$reference))
  graph$percentages[graph$percentages %in% NA] <- 0
  
  # create the plot
  library(ggplot2)
  plot <- ggplot(graph, aes(x = degree, y = as.numeric(percentages), fill = reference, color = color)) +
    # display a scatterplot
    geom_point() +
    # customize legends
    labs(x = "Degree", 
         y = "Identity Percentage", 
         title = "Degree vs Identity Distributions",
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
Degree_VS_IdentityPercentage(LRP27, TDCor)