import geopandas as gpd
import geopy as gp

locator = gp.Nominatim(user_agent="myGeocoder")
location = locator.geocode("1 Boulevard Stiti, Tizi-Ouzou, Algerie")

print("Latitude = {}, Longitude = {}".format(location.latitude, location.longitude))
