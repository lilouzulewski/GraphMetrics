# GraphMetrics

## Description

The GraphMetrics repository contains R scripts for computing and analyzing various graph metrics. These metrics provide insights into the structural and functional properties of graphs and can be applied to various domains such as network analysis, bioinformatics, and social network analysis. Here, our primary focus lies in gene regulatory networks.

### Metrics Included:

1. [**Distribution of Node Degrees**](./metrics_functions/DistributionNodeDegrees.R)
2. [**Percentage of Overall Identity at Degree 1**](./metrics_functions/PercentageOverallIdentityDegree1.R)
3. [**Comparison of Node Degrees vs. Percentage Identity**](./metrics_functions/ComparisonNodeDegreesPercentageIdentity.R)
4. [**Percentage of Overall Identity at Higher Degrees**](./metrics_functions/PercentageOverallIdentityHigherDegrees.R)
5. **Directness/Time Delay Indices Analysis**
6. **Common Clusters Analysis**

## Usage

To use the metrics provided in this repository, follow these steps:

1. Ensure your dataset contains two columns named `from` and `to`, representing the genes connected in the graph without any specific orientation.
2. Use the provided R functions to compute the desired metrics on your dataset.
3. Analyze the results to gain insights into the structural and functional properties of your graph.

## Results

The initial results obtained using these metrics reveal interesting patterns in the graph structure and provide valuable insights into the relationships between genes. For more detailed analysis and interpretation of the results, refer to the [documentation](https://lilouzulewski.github.io/IRD_internship/) provided in the repository.

For more information and examples, please refer to the documentation and sample dataset included in this repository.

## Author

- Lilou Zulewski (**lilou.zulewski01@etu.umontpellier.fr**, **lilou.zulewski@ird.fr**)
