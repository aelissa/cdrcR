# cdrcR

```cdrcR``` is an R wrapper to access the CDRC APIs endpoints and retrieve CDRC data programmatically. 
The package is designed to have one main function -- ```getCDRC``` -- which allows users to get data from all CDRC APIs endpoints. 
A list of the datasets that can be accessed through these enpoints and their metadata is obtained by running ```listCDRC()```. Such list will also provide users with a dataset identifier --the **dataCode**-- which will address the data request to the correct endpoint. Run ```?getCDRC()``` to access the function full documentation.

## Installation

You can install the development version of ```cdrcR``` from Github using devtools.

```
# install.packages("devtools")
devtools::install_github("aelissa/cdrcR")

```

## Register to the CDRC

To use the CDRC APIs you need to register to the CDRC. 
As we are on a testing phase the API registration is not open yet.

##  Usage 

Load the library.
```
library(cdrcR)
```
Log-in with the username and password that you used when registered to the CDRC.

```
loginCDRC(username="your-username",password="your-password")
```
Then you can list the datasets available and the relative dataCode which identifies the API endpoint.

```
listCDRC()

```
which  will result in a data frame like the following extract:

|	Title 	|	DataCode  | dataSetURL | GeographicalCoverage | GeographyLevel	|
|-----------------------------|--------------------------------|--------------------------------------------------------------------------|--------------------|---------|							
| Access to Healthy Assets & Hazards (AHAH) 2019  |      AHAHInputs, AHAHOverallIndexDomain |  https://data.cdrc.ac.uk/dataset/access-healthy-assets-hazards-ahah    |     GreatBritain     |      LSOA         |                    
 |Classification of Workplace Zones (COWZ) 2011    |     	COWZUK2011 | https://data.cdrc.ac.uk/dataset/classification-workplace-zones-cowz     |   UnitedKingdom      |       WZ |                             
|Index of Multiple Deprivation (IMD) 2019           |                        IMD2019 | https://data.cdrc.ac.uk/dataset/index-multiple-deprivation-imd       | UnitedKingdom      |     LSOA                                  
| Internet User Classification (IUC) 2018        |                           IUC2018 |  https://data.cdrc.ac.uk/dataset/internet-user-classification      |   GreatBritain     |      LSOA |


Pick the ```DataCode``` relative to the dataset you want to retreive data about and use it as input for the dataCode parameter in ```getCDRC``` (run ```?getCDRC()``` to see the detailed documentation). 

Be aware that the API endpoints enable query for the following geographies: postcodes, LSOAs and MSOAs. Not all data are originally developed at these geography levels (you can find the original geography level for each dataset) with `listCDRC()`), therefore the areas that better overlap your required geography will be returned.

## Examples

1. Get the overall Access to Healthy Assets & Hazards (AHAH) index for the following postcodes: L13AY,L82TJ,L83UL
For the Access to Healthy Assets & Hazards (AHAH) index you can chose to get either the individual inputs (via AHAHInputs) or the overall domain index (via AHAHOverallIndexDomain).  In this example we are interested in the overall index to rank the postcodes above by their level of access to healthy assets and hazards. Please note that while the request is for postcodes the data is at LSOA level, therefore the LSOAs that better overlap the requested postcodes will be returned. 

```
###login

loginCDRC(username="your-username",password="your-password")

###check dataCode

listCDRC()

###get the AHAH index for  postcodes L13AY,L82TJ,L83UL

ahah<-getCDRC("AHAHOverallIndexDomain",geography = "postcode", geographyCode = c("L13AY","L82TJ","L83UL"))

###rank postcodes by AHAH index from best to worst performing

ahah[order(ahah$ahah),c("postCode","ahah")]

#  postCode     ahah
#3   L8 3UL 20.01734
#2   L8 2TJ 23.04482
#1   L1 3AY 45.91745

```


2. Get the Internet User Classification across Liverpool Local Authority and map it.

```
###login

loginCDRC(username="your-username",password="your-password")

###check dataCode

listCDRC()

###get Liverpool Local Authority District LSOAs  

liverpool<-sf::st_as_sf(liverpool)

###get IUC data with geographical boundaries

iuc<-getCDRC("IUC2018",geography = "LSOA", geographyCode = liverpool$LSOA11CD, boundaries = TRUE)

###map iuc

library(ggplot2)
ggplot()+
geom_sf(aes(fill=grpLabel, group=grpLabel),iuc,show.legend = T,color="NA")+
theme_void()+
labs(fill="IUC groups")

```
