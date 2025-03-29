# Installation
install.packages(c('tidygeocoder' # for geocoding
                   , 'maps' # map data for Visualizations
                   , 'geodist' # calculating distance
                   , 'sf' # interface for spatial data (e.g., shapefiles)
                   , 'tigris' # US Census shapefiles
                   , 'tidycensus' # query US Census data
) 
, repos='https://cran.r-project.org')

library(tidygeocoder)
library(dplyr)

adress = "24 parc de la vallée, 68530 Buhl" %>% as.data.frame()
colnames(adress) <- "add"

geo_code <- adress %>%
geocode(address=add
        , method='osm')
