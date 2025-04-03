
library(sp)
library(sf) 
library(spdep)
## Load in the demographic data and county data for North Carolina
ncdata=read.csv("C:/Users/jacka/Downloads/NCcounty2024data.csv",header=T)
load("C:/Users/jacka/Downloads/nc_counties.Rdata")
## Plot the counties
plot(nc_counties)
## Names of the counties
nc_counties[[7]]
## Obtain neighborhoods given the polygons
nc_neigh=poly2nb(nc_counties)
nc_counties$age=ncdata$Median.Age
spplot(nc_counties,"Opportunity.Youth.2022")

ncdata$County <- tolower(ncdata$County)
nc_counties$NAME_2 <- tolower(nc_counties$NAME_2) 

head(ncdata$County)  # County names in ncdata
head(nc_counties$NAME_2)  

# Merge datasets based on county name (adjust column names if needed)
nc_counties <- merge(nc_counties, ncdata, by.x = "NAME_2", by.y = "County")

# Define spatial neighbors using Queen's Contiguity
nc_nb <- poly2nb(nc_counties, queen = TRUE)


# Convert to a spatial weights list object
nc_listw <- nb2listw(nc_nb, style = "W")  

# Moran's I Test (Global Spatial Autocorrelation)
moran_test <- moran.test(nc_counties$Opportunity.Youth.2022, nc_listw)

# Geary's C Test (Local Spatial Autocorrelation)
geary_test <- geary.test(nc_counties$Opportunity.Youth.2022, nc_listw)

# Print results
print(moran_test)
print(geary_test)
# Moderate positive spatial autocorrelation 
# also significant 
# this result was shown in both moran and geary tests