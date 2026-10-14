# Examination of state-level nonmetropolitan mortality penalty patterns reveal heterogeneity across the nation, 1999-2019
## Introduction

Here I provide the data and code for replicating the findings presented in the data visualization that illustrates heterogeneity in the emergence of the nonmetropolitan mortality penalty in the United States.

## Data
All data are state-level and obtained through CDC Wonder. 

### Codebook

#### Observation Identifiers
* State - US State
* Year - Year of Measure
  
#### Processed file
* MRR - Morality Rate Ratio 
* MRR_Lo - Lower bound of the 95% CI for the MRR
* MRR_Hi - Higher bound of the 95% CI for the MRR
* MRR_Adj - Mortality Rate Ratio removing effect of racial/ethnic composition
* MRR_Lo_Adj - Lower bound of the 95% CI for the MRR Adjusted
* MRR_Hi_Adj - Higher bound of the 95% CI for the MRR Adjusted
 
# Code
The code included in this repository produced the visualization and supporting tabulations.

# Decomposition 
Includes the decomposition process, including harmonized data and processing post raw files.

# Raw files
Includes CDC data extracts used to build data for the decomposition. 

# Correspondence
For any issue with the functionality of this script please create an issue

# License
The data collected and presented is licensed under the Creative Commons Attribution 3.0 license, and the underlying code used to format, analyze and display that content is licensed under the MIT license.
