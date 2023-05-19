# WNV-risk-predictor
Dead Bird Surveillance as a Predictor of Equine Risk for West Nile Virus Infection. Final individual project for Applied Biostatistics  (MATH-493) EPFL

The efficacy of different surveillance methods for predicting and preventing human and veterinary illness from West Nile Virus (WNV) infection in the United States has been argument of debate since its introduction in 1999.This paper will discuss the effect of dead bird surveillance, which has been a focus of particular interest since bird mortality typically precedes human or equine WNV infection. This article will delve into the Poisson regression model of equine WNV (West Nile Virus) rate, examining how it varies based on the rate of WNV-positive dead birds. The analysis will also factor in population density, accounting for potential variations in the impact of population density on WNV rate. The Poisson regression model will demonstrate a strong match with the available data.

The study utilizes the variables described below:
  
  * Equine cases (int): Count variable. Number of WNV-positive equine cases in the specific County.
  * County (str): County area in South Carolina.
  * Bird cases (int): Count data. Number of WNV-positive bird cases in the specific County.
  * Farms (int): number of farms in the specific County.
  * Area (int): area of the County in squared miles.
  * Population (int): population of the specific County.
  * Human density (float): human density of the county computed as Population/Area
  * Positive bird rate (float): (PBR) # Bird Cases of West Nile / Human Population
  * Positive Equine Rate (float): (PER) # of Equine Cases of West Nile / # Farms

These data is coming from a combination of sources, among which the South Carolina Department of Health and Environmental Control, (U.S. Census Bureau 2000, and United States Department of Agriculture’s Census of Agriculture statistics. 
