#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(tidygeocoder)
library(dplyr)


#* Plot a histogram
#* @param addresse
#* @get /geoloc
function(adresse) {
  
  adress = addresse %>% as.data.frame()
  colnames(adress) <- "add"
  
  adress %>%
    geocode(address=add
            , method='osm')
}

48.86892106296816, 2.31013298034668
