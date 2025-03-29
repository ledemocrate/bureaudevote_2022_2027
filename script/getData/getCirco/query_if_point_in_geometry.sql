SELECT * FROM assemblee_elective.circonscription c ;

SELECT 

geometry,
ST_SetSRID(ST_MakePoint(-0.522139761163425, 44.87391347083131 ),4326),
ST_Contains(geometry,ST_SetSRID(ST_MakePoint(-0.522139761163425, 44.87391347083131),4326)) as test

FROM assemblee_elective.circonscription c