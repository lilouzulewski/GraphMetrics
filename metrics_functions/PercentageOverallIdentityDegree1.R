# GLOBAL IDENTITY PERCENTAGE BETWEEN TWO GRAPHS OF DEGREE 1
# formatting the databases:
# columns should be named "from" and "to"

Global_Identity <- function(data1, data2) {
  
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
  data1 <- Sort_Columns(data1)
  data2 <- Sort_Columns(data2)
  
  # concatenate gene pairs into a new dataframe for each dataset
  gene_pairs_data1 <- cbind(from = data1$from, to = data1$to, pairs = paste(data1$from, data1$to, sep = "_"))
  gene_pairs_data2 <- cbind(from = data2$from, to = data2$to, pairs = paste(data2$from, data2$to, sep = "_"))
  
  # remove duplicates in gene pairs
  unique_pairs_data1 <- unique(gene_pairs_data1)
  unique_pairs_data2 <- unique(gene_pairs_data2)
  
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


# EXAMPLE OF USE WITH LR DATASET

# calculate the identity percentages of degree 1 at each time step
LRP6_TDCor <- Global_Identity(LRP6, TDCor)
LRP9_TDCor <- Global_Identity(LRP9, TDCor)
LRP12_TDCor <- Global_Identity(LRP12, TDCor)
LRP15_TDCor <- Global_Identity(LRP15, TDCor)
LRP18_TDCor <- Global_Identity(LRP18, TDCor)
LRP21_TDCor <- Global_Identity(LRP21, TDCor)
LRP24_TDCor <- Global_Identity(LRP24, TDCor)
LRP27_TDCor <- Global_Identity(LRP27, TDCor)
LRP30_TDCor <- Global_Identity(LRP30, TDCor)
LRP33_TDCor <- Global_Identity(LRP33, TDCor)
LRP36_TDCor <- Global_Identity(LRP36, TDCor)
LRP39_TDCor <- Global_Identity(LRP39, TDCor)
LRP42_TDCor <- Global_Identity(LRP42, TDCor)
LRP45_TDCor <- Global_Identity(LRP45, TDCor)
LRP48_TDCor <- Global_Identity(LRP48, TDCor)
LRP51_TDCor <- Global_Identity(LRP51, TDCor)
LRP54_TDCor <- Global_Identity(LRP54, TDCor)

# calculate the global identity percentage of degree 1
LRP_TDCor <- Global_Identity(LR, TDCor)

# DISPLAYING THE RESULTS OF LR DATASET

# create a database containing the results of DIANE inferred network
LRPReference <- c(LRP6_TDCor$identity_percentage_ReferenceData1,
                  LRP9_TDCor$identity_percentage_ReferenceData1,
                  LRP12_TDCor$identity_percentage_ReferenceData1,
                  LRP15_TDCor$identity_percentage_ReferenceData1,
                  LRP18_TDCor$identity_percentage_ReferenceData1,
                  LRP21_TDCor$identity_percentage_ReferenceData1,
                  LRP24_TDCor$identity_percentage_ReferenceData1,
                  LRP27_TDCor$identity_percentage_ReferenceData1,
                  LRP30_TDCor$identity_percentage_ReferenceData1,
                  LRP33_TDCor$identity_percentage_ReferenceData1,
                  LRP36_TDCor$identity_percentage_ReferenceData1,
                  LRP39_TDCor$identity_percentage_ReferenceData1,
                  LRP42_TDCor$identity_percentage_ReferenceData1,
                  LRP45_TDCor$identity_percentage_ReferenceData1,
                  LRP48_TDCor$identity_percentage_ReferenceData1,
                  LRP51_TDCor$identity_percentage_ReferenceData1,
                  LRP54_TDCor$identity_percentage_ReferenceData1)

# create a database containing the results of TDCor inferred network
TDCorReference <- c(LRP6_TDCor$identity_percentage_ReferenceData2,
                    LRP9_TDCor$identity_percentage_ReferenceData2,
                    LRP12_TDCor$identity_percentage_ReferenceData2,
                    LRP15_TDCor$identity_percentage_ReferenceData2,
                    LRP18_TDCor$identity_percentage_ReferenceData2,
                    LRP21_TDCor$identity_percentage_ReferenceData2,
                    LRP24_TDCor$identity_percentage_ReferenceData2,
                    LRP27_TDCor$identity_percentage_ReferenceData2,
                    LRP30_TDCor$identity_percentage_ReferenceData2,
                    LRP33_TDCor$identity_percentage_ReferenceData2,
                    LRP36_TDCor$identity_percentage_ReferenceData2,
                    LRP39_TDCor$identity_percentage_ReferenceData2,
                    LRP42_TDCor$identity_percentage_ReferenceData2,
                    LRP45_TDCor$identity_percentage_ReferenceData2,
                    LRP48_TDCor$identity_percentage_ReferenceData2,
                    LRP51_TDCor$identity_percentage_ReferenceData2,
                    LRP54_TDCor$identity_percentage_ReferenceData2)

# create a dataframe containing the results
GlobalIdentityResults <- data.frame(
  time_steps = c(seq(6, 54, by=3), 57),
  label = c(rep("", length(LRPReference)), "global", rep("", length(TDCorReference)), "global"), # Adding a label column
  reference = c(rep("LRP", length(LRPReference)+1), rep("TDCor", length(TDCorReference)+1)),
  percentages = c(LRPReference, 7.272727, TDCorReference, 6.25)
)

# graph comparing the identity percentages of degree 1 at each time step
ggplot(GlobalIdentityResults, aes(x=time_steps, y=percentages, color=reference, label=label)) +
  # display different type of plots
  geom_line(data = subset(GlobalIdentityResults, time_steps <= 54)) +
  geom_point(size=0.5, color="black") +
  geom_point(data = subset(GlobalIdentityResults, time_steps %in% 57), size=1) +
  # customize scales
  scale_x_continuous(breaks=c(seq(6, 54, by = 3), 57),
                     labels=c(seq(6, 54, by = 3), "global")) +
  scale_y_continuous(breaks=seq(0, 15, by = 3)) +
  # customize legends
  labs(x="Time Steps",
       y="Identity Percentage",
       title="Inference identity percentage over time",
       color=expression(underline("Reference"))) +
  # customize text elements
  theme(strip.text.x=element_text(size=6),
        plot.title=element_text(size=12, hjust=0.5, face="bold"),
        legend.title=element_text(size=10),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7))



# EXAMPLE OF USE WITH DIANE INFERRED NETWORK OF LR DATASET

DIANEglobalidentity <- data.frame(matrix(nrow=length(seq(6, 54, by=3)), 
                                         ncol=length(seq(6, 54, by=3)), 
                                         dimnames=list(seq(6, 54, by=3), seq(6, 54, by=3))))

names(DIANEglobalidentity) <- gsub("^X", "", names(DIANEglobalidentity))

DIANEglobalidentity[1,1] <- Global_Identity(LRP6,LRP6)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,2] <- Global_Identity(LRP6,LRP9)$identity_percentage_ReferenceData1
DIANEglobalidentity[2,1] <- Global_Identity(LRP6,LRP9)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,2] <- Global_Identity(LRP9,LRP9)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,3] <- Global_Identity(LRP6,LRP12)$identity_percentage_ReferenceData1
DIANEglobalidentity[3,1] <- Global_Identity(LRP6,LRP12)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,3] <- Global_Identity(LRP12,LRP12)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,4] <- Global_Identity(LRP6,LRP15)$identity_percentage_ReferenceData1
DIANEglobalidentity[4,1] <- Global_Identity(LRP6,LRP15)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,4] <- Global_Identity(LRP15,LRP15)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,5] <- Global_Identity(LRP6,LRP18)$identity_percentage_ReferenceData1
DIANEglobalidentity[5,1] <- Global_Identity(LRP6,LRP18)$identity_percentage_ReferenceData2
DIANEglobalidentity[5,5] <- Global_Identity(LRP18,LRP18)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,6] <- Global_Identity(LRP6,LRP21)$identity_percentage_ReferenceData1
DIANEglobalidentity[6,1] <- Global_Identity(LRP6,LRP21)$identity_percentage_ReferenceData2
DIANEglobalidentity[6,6] <- Global_Identity(LRP21,LRP21)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,7] <- Global_Identity(LRP6,LRP24)$identity_percentage_ReferenceData1
DIANEglobalidentity[7,1] <- Global_Identity(LRP6,LRP24)$identity_percentage_ReferenceData2
DIANEglobalidentity[7,7] <- Global_Identity(LRP24,LRP24)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,8] <- Global_Identity(LRP6,LRP27)$identity_percentage_ReferenceData1
DIANEglobalidentity[8,1] <- Global_Identity(LRP6,LRP27)$identity_percentage_ReferenceData2
DIANEglobalidentity[8,8] <- Global_Identity(LRP27,LRP27)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,9] <- Global_Identity(LRP6,LRP30)$identity_percentage_ReferenceData1
DIANEglobalidentity[9,1] <- Global_Identity(LRP6,LRP30)$identity_percentage_ReferenceData2
DIANEglobalidentity[9,9] <- Global_Identity(LRP30,LRP30)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,10] <- Global_Identity(LRP6,LRP33)$identity_percentage_ReferenceData1
DIANEglobalidentity[10,1] <- Global_Identity(LRP6,LRP33)$identity_percentage_ReferenceData2
DIANEglobalidentity[10,10] <- Global_Identity(LRP33,LRP33)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,11] <- Global_Identity(LRP6,LRP36)$identity_percentage_ReferenceData1
DIANEglobalidentity[11,1] <- Global_Identity(LRP6,LRP36)$identity_percentage_ReferenceData2
DIANEglobalidentity[11,11] <- Global_Identity(LRP36,LRP36)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,12] <- Global_Identity(LRP6,LRP39)$identity_percentage_ReferenceData1
DIANEglobalidentity[12,1] <- Global_Identity(LRP6,LRP39)$identity_percentage_ReferenceData2
DIANEglobalidentity[12,12] <- Global_Identity(LRP39,LRP39)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,13] <- Global_Identity(LRP6,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[13,1] <- Global_Identity(LRP6,LRP42)$identity_percentage_ReferenceData2
DIANEglobalidentity[13,13] <- Global_Identity(LRP42,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,14] <- Global_Identity(LRP6,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,1] <- Global_Identity(LRP6,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[14,14] <- Global_Identity(LRP45,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,15] <- Global_Identity(LRP6,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,1] <- Global_Identity(LRP6,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[15,15] <- Global_Identity(LRP48,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,16] <- Global_Identity(LRP6,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,1] <- Global_Identity(LRP6,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[16,16] <- Global_Identity(LRP51,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[1,17] <- Global_Identity(LRP6,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,1] <- Global_Identity(LRP6,LRP54)$identity_percentage_ReferenceData2
DIANEglobalidentity[17,17] <- Global_Identity(LRP54,LRP54)$identity_percentage_ReferenceData1

DIANEglobalidentity[2,3] <- Global_Identity(LRP9,LRP12)$identity_percentage_ReferenceData1
DIANEglobalidentity[3,2] <- Global_Identity(LRP9,LRP12)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,4] <- Global_Identity(LRP9,LRP15)$identity_percentage_ReferenceData1
DIANEglobalidentity[4,2] <- Global_Identity(LRP9,LRP15)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,5] <- Global_Identity(LRP9,LRP18)$identity_percentage_ReferenceData1
DIANEglobalidentity[5,2] <- Global_Identity(LRP9,LRP18)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,6] <- Global_Identity(LRP9,LRP21)$identity_percentage_ReferenceData1
DIANEglobalidentity[6,2] <- Global_Identity(LRP9,LRP21)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,7] <- Global_Identity(LRP9,LRP24)$identity_percentage_ReferenceData1
DIANEglobalidentity[7,2] <- Global_Identity(LRP9,LRP24)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,8] <- Global_Identity(LRP9,LRP27)$identity_percentage_ReferenceData1
DIANEglobalidentity[8,2] <- Global_Identity(LRP9,LRP27)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,9] <- Global_Identity(LRP9,LRP30)$identity_percentage_ReferenceData1
DIANEglobalidentity[9,2] <- Global_Identity(LRP9,LRP30)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,10] <- Global_Identity(LRP9,LRP33)$identity_percentage_ReferenceData1
DIANEglobalidentity[10,2] <- Global_Identity(LRP9,LRP33)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,11] <- Global_Identity(LRP9,LRP36)$identity_percentage_ReferenceData1
DIANEglobalidentity[11,2] <- Global_Identity(LRP9,LRP36)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,12] <- Global_Identity(LRP9,LRP39)$identity_percentage_ReferenceData1
DIANEglobalidentity[12,2] <- Global_Identity(LRP9,LRP39)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,13] <- Global_Identity(LRP9,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[13,2] <- Global_Identity(LRP9,LRP42)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,14] <- Global_Identity(LRP9,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,2] <- Global_Identity(LRP9,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,15] <- Global_Identity(LRP9,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,2] <- Global_Identity(LRP9,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,16] <- Global_Identity(LRP9,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,2] <- Global_Identity(LRP9,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[2,17] <- Global_Identity(LRP9,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,2] <- Global_Identity(LRP9,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[3,4] <- Global_Identity(LRP12,LRP15)$identity_percentage_ReferenceData1
DIANEglobalidentity[4,3] <- Global_Identity(LRP12,LRP15)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,5] <- Global_Identity(LRP12,LRP18)$identity_percentage_ReferenceData1
DIANEglobalidentity[5,3] <- Global_Identity(LRP12,LRP18)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,6] <- Global_Identity(LRP12,LRP21)$identity_percentage_ReferenceData1
DIANEglobalidentity[6,3] <- Global_Identity(LRP12,LRP21)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,7] <- Global_Identity(LRP12,LRP24)$identity_percentage_ReferenceData1
DIANEglobalidentity[7,3] <- Global_Identity(LRP12,LRP24)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,8] <- Global_Identity(LRP12,LRP27)$identity_percentage_ReferenceData1
DIANEglobalidentity[8,3] <- Global_Identity(LRP12,LRP27)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,9] <- Global_Identity(LRP12,LRP30)$identity_percentage_ReferenceData1
DIANEglobalidentity[9,3] <- Global_Identity(LRP12,LRP30)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,10] <- Global_Identity(LRP12,LRP33)$identity_percentage_ReferenceData1
DIANEglobalidentity[10,3] <- Global_Identity(LRP12,LRP33)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,11] <- Global_Identity(LRP12,LRP36)$identity_percentage_ReferenceData1
DIANEglobalidentity[11,3] <- Global_Identity(LRP12,LRP36)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,12] <- Global_Identity(LRP12,LRP39)$identity_percentage_ReferenceData1
DIANEglobalidentity[12,3] <- Global_Identity(LRP12,LRP39)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,13] <- Global_Identity(LRP12,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[13,3] <- Global_Identity(LRP12,LRP42)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,14] <- Global_Identity(LRP12,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,3] <- Global_Identity(LRP12,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,15] <- Global_Identity(LRP12,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,3] <- Global_Identity(LRP12,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,16] <- Global_Identity(LRP12,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,3] <- Global_Identity(LRP12,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[3,17] <- Global_Identity(LRP12,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,3] <- Global_Identity(LRP12,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[4,5] <- Global_Identity(LRP15,LRP18)$identity_percentage_ReferenceData1
DIANEglobalidentity[5,4] <- Global_Identity(LRP15,LRP18)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,6] <- Global_Identity(LRP15,LRP21)$identity_percentage_ReferenceData1
DIANEglobalidentity[6,4] <- Global_Identity(LRP15,LRP21)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,7] <- Global_Identity(LRP15,LRP24)$identity_percentage_ReferenceData1
DIANEglobalidentity[7,4] <- Global_Identity(LRP15,LRP24)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,8] <- Global_Identity(LRP15,LRP27)$identity_percentage_ReferenceData1
DIANEglobalidentity[8,4] <- Global_Identity(LRP15,LRP27)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,9] <- Global_Identity(LRP15,LRP30)$identity_percentage_ReferenceData1
DIANEglobalidentity[9,4] <- Global_Identity(LRP15,LRP30)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,10] <- Global_Identity(LRP15,LRP33)$identity_percentage_ReferenceData1
DIANEglobalidentity[10,4] <- Global_Identity(LRP15,LRP33)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,11] <- Global_Identity(LRP15,LRP36)$identity_percentage_ReferenceData1
DIANEglobalidentity[11,4] <- Global_Identity(LRP15,LRP36)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,12] <- Global_Identity(LRP15,LRP39)$identity_percentage_ReferenceData1
DIANEglobalidentity[12,4] <- Global_Identity(LRP15,LRP39)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,13] <- Global_Identity(LRP15,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[13,4] <- Global_Identity(LRP15,LRP42)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,14] <- Global_Identity(LRP15,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,4] <- Global_Identity(LRP15,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,15] <- Global_Identity(LRP15,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,4] <- Global_Identity(LRP15,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,16] <- Global_Identity(LRP15,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,4] <- Global_Identity(LRP15,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[4,17] <- Global_Identity(LRP15,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,4] <- Global_Identity(LRP15,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[5,6] <- Global_Identity(LRP18,LRP21)$identity_percentage_ReferenceData1
DIANEglobalidentity[6,5] <- Global_Identity(LRP18,LRP21)$identity_percentage_ReferenceData2
DIANEglobalidentity[5,7] <- Global_Identity(LRP18,LRP24)$identity_percentage_ReferenceData1
DIANEglobalidentity[7,5] <- Global_Identity(LRP18,LRP24)$identity_percentage_ReferenceData2
DIANEglobalidentity[5,8] <- Global_Identity(LRP18,LRP27)$identity_percentage_ReferenceData1
DIANEglobalidentity[8,5] <- Global_Identity(LRP18,LRP27)$identity_percentage_ReferenceData2
DIANEglobalidentity[5,9] <- Global_Identity(LRP18,LRP30)$identity_percentage_ReferenceData1
DIANEglobalidentity[9,5] <- Global_Identity(LRP18,LRP30)$identity_percentage_ReferenceData2
DIANEglobalidentity[5,10] <- Global_Identity(LRP18,LRP33)$identity_percentage_ReferenceData1
DIANEglobalidentity[10,5] <- Global_Identity(LRP18,LRP33)$identity_percentage_ReferenceData2
DIANEglobalidentity[5,11] <- Global_Identity(LRP18,LRP36)$identity_percentage_ReferenceData1
DIANEglobalidentity[11,5] <- Global_Identity(LRP18,LRP36)$identity_percentage_ReferenceData2
DIANEglobalidentity[5,12] <- Global_Identity(LRP18,LRP39)$identity_percentage_ReferenceData1
DIANEglobalidentity[12,5] <- Global_Identity(LRP18,LRP39)$identity_percentage_ReferenceData2
DIANEglobalidentity[5,13] <- Global_Identity(LRP18,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[13,5] <- Global_Identity(LRP18,LRP42)$identity_percentage_ReferenceData2
DIANEglobalidentity[5,14] <- Global_Identity(LRP18,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,5] <- Global_Identity(LRP18,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[5,15] <- Global_Identity(LRP18,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,5] <- Global_Identity(LRP18,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[5,16] <- Global_Identity(LRP18,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,5] <- Global_Identity(LRP18,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[5,17] <- Global_Identity(LRP18,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,5] <- Global_Identity(LRP18,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[6,7] <- Global_Identity(LRP21,LRP24)$identity_percentage_ReferenceData1
DIANEglobalidentity[7,6] <- Global_Identity(LRP21,LRP24)$identity_percentage_ReferenceData2
DIANEglobalidentity[6,8] <- Global_Identity(LRP21,LRP27)$identity_percentage_ReferenceData1
DIANEglobalidentity[8,6] <- Global_Identity(LRP21,LRP27)$identity_percentage_ReferenceData2
DIANEglobalidentity[6,9] <- Global_Identity(LRP21,LRP30)$identity_percentage_ReferenceData1
DIANEglobalidentity[9,6] <- Global_Identity(LRP21,LRP30)$identity_percentage_ReferenceData2
DIANEglobalidentity[6,10] <- Global_Identity(LRP21,LRP33)$identity_percentage_ReferenceData1
DIANEglobalidentity[10,6] <- Global_Identity(LRP21,LRP33)$identity_percentage_ReferenceData2
DIANEglobalidentity[6,11] <- Global_Identity(LRP21,LRP36)$identity_percentage_ReferenceData1
DIANEglobalidentity[11,6] <- Global_Identity(LRP21,LRP36)$identity_percentage_ReferenceData2
DIANEglobalidentity[6,12] <- Global_Identity(LRP21,LRP39)$identity_percentage_ReferenceData1
DIANEglobalidentity[12,6] <- Global_Identity(LRP21,LRP39)$identity_percentage_ReferenceData2
DIANEglobalidentity[6,13] <- Global_Identity(LRP21,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[13,6] <- Global_Identity(LRP21,LRP42)$identity_percentage_ReferenceData2
DIANEglobalidentity[6,14] <- Global_Identity(LRP21,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,6] <- Global_Identity(LRP21,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[6,15] <- Global_Identity(LRP21,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,6] <- Global_Identity(LRP21,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[6,16] <- Global_Identity(LRP21,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,6] <- Global_Identity(LRP21,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[6,17] <- Global_Identity(LRP21,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,6] <- Global_Identity(LRP21,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[7,8] <- Global_Identity(LRP24,LRP27)$identity_percentage_ReferenceData1
DIANEglobalidentity[8,7] <- Global_Identity(LRP24,LRP27)$identity_percentage_ReferenceData2
DIANEglobalidentity[7,9] <- Global_Identity(LRP24,LRP30)$identity_percentage_ReferenceData1
DIANEglobalidentity[9,7] <- Global_Identity(LRP24,LRP30)$identity_percentage_ReferenceData2
DIANEglobalidentity[7,10] <- Global_Identity(LRP24,LRP33)$identity_percentage_ReferenceData1
DIANEglobalidentity[10,7] <- Global_Identity(LRP24,LRP33)$identity_percentage_ReferenceData2
DIANEglobalidentity[7,11] <- Global_Identity(LRP24,LRP36)$identity_percentage_ReferenceData1
DIANEglobalidentity[11,7] <- Global_Identity(LRP24,LRP36)$identity_percentage_ReferenceData2
DIANEglobalidentity[7,12] <- Global_Identity(LRP24,LRP39)$identity_percentage_ReferenceData1
DIANEglobalidentity[12,7] <- Global_Identity(LRP24,LRP39)$identity_percentage_ReferenceData2
DIANEglobalidentity[7,13] <- Global_Identity(LRP24,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[13,7] <- Global_Identity(LRP24,LRP42)$identity_percentage_ReferenceData2
DIANEglobalidentity[7,14] <- Global_Identity(LRP24,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,7] <- Global_Identity(LRP24,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[7,15] <- Global_Identity(LRP24,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,7] <- Global_Identity(LRP24,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[7,16] <- Global_Identity(LRP24,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,7] <- Global_Identity(LRP24,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[7,17] <- Global_Identity(LRP24,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,7] <- Global_Identity(LRP24,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[8,9] <- Global_Identity(LRP27,LRP30)$identity_percentage_ReferenceData1
DIANEglobalidentity[9,8] <- Global_Identity(LRP27,LRP30)$identity_percentage_ReferenceData2
DIANEglobalidentity[8,10] <- Global_Identity(LRP27,LRP33)$identity_percentage_ReferenceData1
DIANEglobalidentity[10,8] <- Global_Identity(LRP27,LRP33)$identity_percentage_ReferenceData2
DIANEglobalidentity[8,11] <- Global_Identity(LRP27,LRP36)$identity_percentage_ReferenceData1
DIANEglobalidentity[11,8] <- Global_Identity(LRP27,LRP36)$identity_percentage_ReferenceData2
DIANEglobalidentity[8,12] <- Global_Identity(LRP27,LRP39)$identity_percentage_ReferenceData1
DIANEglobalidentity[12,8] <- Global_Identity(LRP27,LRP39)$identity_percentage_ReferenceData2
DIANEglobalidentity[8,13] <- Global_Identity(LRP27,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[13,8] <- Global_Identity(LRP27,LRP42)$identity_percentage_ReferenceData2
DIANEglobalidentity[8,14] <- Global_Identity(LRP27,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,8] <- Global_Identity(LRP27,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[8,15] <- Global_Identity(LRP27,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,8] <- Global_Identity(LRP27,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[8,16] <- Global_Identity(LRP27,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,8] <- Global_Identity(LRP27,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[8,17] <- Global_Identity(LRP27,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,8] <- Global_Identity(LRP27,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[9,10] <- Global_Identity(LRP30,LRP33)$identity_percentage_ReferenceData1
DIANEglobalidentity[10,9] <- Global_Identity(LRP30,LRP33)$identity_percentage_ReferenceData2
DIANEglobalidentity[9,11] <- Global_Identity(LRP30,LRP36)$identity_percentage_ReferenceData1
DIANEglobalidentity[11,9] <- Global_Identity(LRP30,LRP36)$identity_percentage_ReferenceData2
DIANEglobalidentity[9,12] <- Global_Identity(LRP30,LRP39)$identity_percentage_ReferenceData1
DIANEglobalidentity[12,9] <- Global_Identity(LRP30,LRP39)$identity_percentage_ReferenceData2
DIANEglobalidentity[9,13] <- Global_Identity(LRP30,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[13,9] <- Global_Identity(LRP30,LRP42)$identity_percentage_ReferenceData2
DIANEglobalidentity[9,14] <- Global_Identity(LRP30,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,9] <- Global_Identity(LRP30,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[9,15] <- Global_Identity(LRP30,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,9] <- Global_Identity(LRP30,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[9,16] <- Global_Identity(LRP30,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,9] <- Global_Identity(LRP30,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[9,17] <- Global_Identity(LRP30,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,9] <- Global_Identity(LRP30,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[10,11] <- Global_Identity(LRP33,LRP36)$identity_percentage_ReferenceData1
DIANEglobalidentity[11,10] <- Global_Identity(LRP33,LRP36)$identity_percentage_ReferenceData2
DIANEglobalidentity[10,12] <- Global_Identity(LRP33,LRP39)$identity_percentage_ReferenceData1
DIANEglobalidentity[12,10] <- Global_Identity(LRP33,LRP39)$identity_percentage_ReferenceData2
DIANEglobalidentity[10,13] <- Global_Identity(LRP33,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[13,10] <- Global_Identity(LRP33,LRP42)$identity_percentage_ReferenceData2
DIANEglobalidentity[10,14] <- Global_Identity(LRP33,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,10] <- Global_Identity(LRP33,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[10,15] <- Global_Identity(LRP33,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,10] <- Global_Identity(LRP33,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[10,16] <- Global_Identity(LRP33,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,10] <- Global_Identity(LRP33,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[10,17] <- Global_Identity(LRP33,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,10] <- Global_Identity(LRP33,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[11,12] <- Global_Identity(LRP36,LRP39)$identity_percentage_ReferenceData1
DIANEglobalidentity[12,11] <- Global_Identity(LRP36,LRP39)$identity_percentage_ReferenceData2
DIANEglobalidentity[11,13] <- Global_Identity(LRP36,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[13,11] <- Global_Identity(LRP36,LRP42)$identity_percentage_ReferenceData2
DIANEglobalidentity[11,14] <- Global_Identity(LRP36,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,11] <- Global_Identity(LRP36,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[11,15] <- Global_Identity(LRP36,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,11] <- Global_Identity(LRP36,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[11,16] <- Global_Identity(LRP36,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,11] <- Global_Identity(LRP36,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[11,17] <- Global_Identity(LRP36,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,11] <- Global_Identity(LRP36,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[12,13] <- Global_Identity(LRP39,LRP42)$identity_percentage_ReferenceData1
DIANEglobalidentity[13,12] <- Global_Identity(LRP39,LRP42)$identity_percentage_ReferenceData2
DIANEglobalidentity[12,14] <- Global_Identity(LRP39,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,12] <- Global_Identity(LRP39,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[12,15] <- Global_Identity(LRP39,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,12] <- Global_Identity(LRP39,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[12,16] <- Global_Identity(LRP39,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,12] <- Global_Identity(LRP39,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[12,17] <- Global_Identity(LRP39,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,12] <- Global_Identity(LRP39,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[13,14] <- Global_Identity(LRP42,LRP45)$identity_percentage_ReferenceData1
DIANEglobalidentity[14,13] <- Global_Identity(LRP42,LRP45)$identity_percentage_ReferenceData2
DIANEglobalidentity[13,15] <- Global_Identity(LRP42,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,13] <- Global_Identity(LRP42,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[13,16] <- Global_Identity(LRP42,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,13] <- Global_Identity(LRP42,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[13,17] <- Global_Identity(LRP42,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,13] <- Global_Identity(LRP42,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[14,15] <- Global_Identity(LRP45,LRP48)$identity_percentage_ReferenceData1
DIANEglobalidentity[15,14] <- Global_Identity(LRP45,LRP48)$identity_percentage_ReferenceData2
DIANEglobalidentity[14,16] <- Global_Identity(LRP45,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,14] <- Global_Identity(LRP45,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[14,17] <- Global_Identity(LRP45,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,14] <- Global_Identity(LRP45,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[15,16] <- Global_Identity(LRP48,LRP51)$identity_percentage_ReferenceData1
DIANEglobalidentity[16,15] <- Global_Identity(LRP48,LRP51)$identity_percentage_ReferenceData2
DIANEglobalidentity[15,17] <- Global_Identity(LRP48,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,15] <- Global_Identity(LRP48,LRP54)$identity_percentage_ReferenceData2

DIANEglobalidentity[16,17] <- Global_Identity(LRP51,LRP54)$identity_percentage_ReferenceData1
DIANEglobalidentity[17,16] <- Global_Identity(LRP51,LRP54)$identity_percentage_ReferenceData2

write.table(DIANEglobalidentity, file="DIANEglobalidentity.csv", row.names=TRUE, col.names=TRUE)
