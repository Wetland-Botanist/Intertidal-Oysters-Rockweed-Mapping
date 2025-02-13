# **Distribution of Rockweed Macroalgae (Ascophyllum and Fucus sp) and Intertidal Eastern Oysters (Crassostrea virginica) in the Great Bay - Piscataqua River Estuary, New Hampshire**

## Investigators
Ray Grizzle, David Burdick, Krystin Ward, Gregg Moore, Lauren White, and Grant McKown

## Organization 
Jackson Estuarine Laboratory, School of Marine Sciences & Ocean Engineering, University of New Hampshire

## Contact
Grant McKown, james.mckown@usnh.edu or jgrantmck@gmail.com


## Project Description

Rockweed macroalgae (Ascophyllum and Fucus sp.) was mapped through automated classification in Google Earth Engine usuing 2015 Leaf-off Imagery and 2012 - 2016 NAIP imagery. Thresholds were assigned to pre-selected remote sensing indices (Brown Algae Index, Water Index, Red - Blue Ratio, and Elevation). Preliminary classification of rockweed distribution was then refined through boat field surveys to remove erroneously classified salt marshes and overhanging canopy, which is inherent to the classification process. Accuracy assessments were carried out (n = 230 points) across the estuary and an overall accuracy of 90.5% was observed. Field survey sites (n = 25) were extracted from rockweed distribution and surveyed for eastern oyster and mussel use underneath rockweed canopy. Salinity metrics for each region of Great Bay were calculated from SWMP water quality datasondes between 2015 - 2023. 

## Code Description

R script and input datasets were used to accomplish the following tasks:
1) Calculate descriptive statistics and visualize spectral profiles to determine thresholds for rockweed classification
2) Format, organize, and calculate salinity parameters from water quality datasondes
3) Statistical analysis of relationships between regional, site, and plot-level of oyster, rockweed, and water quality metrics

## Notes on Datasets, Google Earth Engine, and More!
1) Google Earth Engine code for rockweed classification (including geospatial assets) are publicly-available at: https://code.earthengine.google.com/3a3413ab29d03d866cfe6d067b6e969f
2) Geospatial datasets and comprehensive montioring datset of the entire project can be found on FigShare:
3) An individual shapefile with accompanying metadata of Rockweed Distribution can be found at NH Granit: 

## Funding Acknowledgments 
Great Bay 2030, through New Hampshire Charitable Foundation, funded the work of this project.


## Folders
1) R Script: Location of R scripts across analyses of spectral profiles, rockweed - oysters, and water quality
2) Water Quality Analysis - folder for input data, output statistics, and output figures of water quality datasonde salinity analysis
3) Rockweed Analysis - input data, output statistics, formatted datasetes, and output figures of statistical analysis of relationships between rockweed, oysters, and salinity
4) Spectral Analysis - input data, output statistics, and output figures of analysis of spectral profiles from training polygons of 2015 Leaf off Imagery and 2012 - 2016 NAIP imagery

## Script
### Script 1 - Spectral Profile Analysis 
- Calculates the remote sensing indices from the randomly chosen pixels of training polygons (Tree Canopy, Salt Marsh, Rockweed, Open Water, and Bare Mud)
- Calculates descriptive statistics of each remote sensing idex for each category
- Visualizes the descriptive statitics of the spectral profiles and indices of each category
- Conducts on 2015 Leaf-off Imagery and each year of NAIP (2012 - 2016)

### Script 2 - Water Quality Processing
- Organizes, formats, and merges individual years of the same water quality datasonde
- Calculates mean, minimum, and maximum salinity for each month
- Calculates the monthly mean of the mean, minimum, and maximum salinity
- Calcultes the maximum number of consecutive days 10 psu was reached for each year, then calculates the annual mean of days

### Script 3 - Oyster Live Histogram
- Visualizes the size - frequncy of the height of eastern oysters, blue mussels, and ribbed mussels from plot-level field work

### Script 4 - Oyster - Rockweed Format and Summarise
- Formatas and organizes the plot-level dataset of the field work portion of the project including rockweed, elevation, and bivalve metrics
- Calculates means and standard error of all metrics at the (1) site-level and (2) region-level
- Note - the exported datasets from this script are NOT the input datasets of Scripts 5 & 6. Additional data and removal of standard error were compiled into the datasets in Excel for regression purposes (distances, water quality parameters)

### Script 5 - Oyster - Rockweed Site Statistical Analysis
- Formats and organizes the compiled site-level dataset
- Statistical regressions of rockweed and oyster metrics to (1) Distance to Estuary Mouth and (2) Distance to nearest oyster reef or oyster farm in the estuary

### Script 6 - Oyster - Rockweed Region Statistical Analysis
- Formats and organizes the compiled region-level dataset
- Statistical regressions of water quality metrics to rockweed area distribution (from classification)

### Script 7 - Oyster - Rockweed Plot Statistical Analysis
- Statistical regressions of oyster, rockweed, and elevation metrics at the plot-level

Any questions about the comprehensive dataset, input datasets, and R-scripts can be directed to Grant McKown (james.mckown@usnh.edu or jgrantmck@gmail.com). 

Go Georgia Dawgs and Tennessee Vols!

