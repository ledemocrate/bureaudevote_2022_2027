import geopandas as gpd
import os
import psycopg2
from sqlalchemy import create_engine
import geopy as gp

##########
# EVITER CETTE PARTIE CAR MARCHE UNIQUEMENT SUR MON PC
path = os.getcwd() 
path = 'C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027'
# EVITER CETTE PARTIE CAR MARCHE UNIQUEMENT SUR MON PC
##########


db = 'bdd_democratie'  
host_db = '176.147.16.188'   
db_port = '5432'  
db_user = "postgres"  
db_password = 'postgres'


conn = psycopg2.connect(
    host=host_db,
    database=db,
    user=db_user,
    port = db_port,
    password=db_password)

##########
# EVITER CETTE PARTIE CAR MARCHE UNIQUEMENT SUR MON PC
engine = create_engine("postgresql://"+ db_user + ":" + db_password + "@" + host_db + ":" + db_port + "/" + db)  
#CHARGEMENT DES DONNEES VOTE
# Endroit ou vous mettez les fichiers json en telechargeant sous le lien 

os.listdir(path + "/data/data_crico")

gdf = gpd.read_file(os.listdir()[0],driver='GeoJSON',engine="fiona")
gdf.to_postgis("circonscription", engine, schema="assemblee_elective", if_exists='replace', index=False)
##########


adress = "24 parc de la vallée, 68530 Buhl"
locator = gp.Nominatim(user_agent="myGeocoder")
location = locator.geocode(adress)
location.longitude
location.latitude


query_param = "ST_SetSRID(ST_MakePoint(" + str(location.longitude)  + " , " + str(location.latitude) + "),4326)"
query_param
query_kernel = "SELECT nom_dpt, geometry as circonscription, " +  query_param  + " as point, ST_Contains(geometry," + query_param + ") as point_in_circo FROM assemblee_elective.circonscription;"
query_kernel

cursor = conn.cursor()
cursor.execute(query_kernel)
print("Voci le résultat ci-dessous : ")
mobile_records = cursor.fetchall()
mobile_records[292] #Mon adresse appartient bien à la circonscription du HAUT-RHIN
mobile_records[132] # Mon adresse n'appartient pas à ces traitres du BAS-RHIN, honte à vous les Strasbourgeois !!!!!!

#RESTE A PARSER LA REPONSE

for row in mobile_records:
    print("Departement = ", row[0], )
    print("Model = ", adress)
    print("Valeur  = ", row[3], "\n")
    
for row in mobile_records[292:300]:
    print("Departement = ", row[0], )
    print("Model = ", adress)
    print("Valeur  = ", row[3], "\n")

