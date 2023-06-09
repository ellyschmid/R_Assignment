## README
## Salinization of Lake Urmia
### Area of Intrest
The Area of Interest is the Urmia Lake BA#asin in northwest Iran. It's the
largest inland lake of Iran and the world's second largest hyper-saline lake. In 
recent decades the surface area of the lake has ranged between 4600 and 6000 km²
and lost the majority of his water body mainly due to construction of more than 
50 dams for water removel for Irrigation and drinking water supply. The high 
number of aquaculture facilities have disrupted the groundwater inflow into the 
lake. Since 2000, Urmia Lake has lost more than 65–85% of its surface area, 
exposing an extensive area of salt flats.

### Motivation 
Soil salinization is a major cause of soil degradation, which leads to decreased 
soil fertility and significantly contributes to desertification processes and is 
one of the major environmental challenges threatening global food production and 
agricultural sustainability, especially in arid and semi-arid climates. Therefore
is a need to recognize and monitor regions that are highly susceptible to 
salinization and degradation. 

### Main objective
The main objective was to assess the overall change and temporal and spatial 
variability of salt affected areas and development of the water body between 
2000 and 2022 in Urmia lake Basin. 

### Methods
Previous Studies have shown the usability of soil salinity indices derived from 
the visible and near-infrared (NIR) bands of satellite images  for the creation 
of soil salinity/salinization maps. The data used in this project were Landsat 
time-series (2000-2022) images using a five-year interval. 

1. Methodology
  + Derivation of eight soil salinity indicies from the Landsat images (Figure)
  + Random forest classification to assess the temporal and spatial variability of salt         affected land classes
  + Selection of training data based on the year 2015, using a k-means classification of 5     classes and results of the salinity indices  

![alt text](https://github.com/ellyschmid/R_Assignment/blob/main/Indicies.png?raw=true)

Figure: Salinity Indicies used in the Classification </br>
From: Feizizadeh, B.; Omarzadeh, D.; Mohammadzadeh Alajujeh, K.; Blaschke, T.; Makki, M. Impacts of the Urmia Lake Drought on Soil Salinity and Degradation Risk: An Integrated Geoinformatics Analysis and Monitoring Approach. Remote Sens. 2022, 14, 3407.

### R-Project Salinization Classification
The Project contains 3 Scripts: The Functions-Script contains all functions
used for deriving the Salinity Indices from Satellite Images.
The RF_Preparation_Model-Script contains the Preparation steps for the classification by loading the data, applying the Salinity Indices to the Landsat Images, the training of the Random Forest Model and the prediction of the Images. The Visualization-Script contains the
plot for a chord-diagram displaying the class transitions of the salinity classes 
between the first and the last predicted image, a plot displaying the classified 
images and a plot displaying the change in classes between the first and last image. (view results above) 
