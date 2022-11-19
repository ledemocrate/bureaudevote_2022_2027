# bureaudevote

Bureau de vote en ligne sur les lois proposées par l'Assemblée Nationale.

- La récupération des votes, des informations sur les députés, et les textes de loi sur les divers site de l'opendata se fait via le logiciel libre Rstudio

- La présentation de résultats se fait via Rshiny, la partie de récupération de vote et d'émargement se fait via respectivement :
  l'API Google Sheet & l'API Google Mail. 
  
Il est à faire remarquer que pour l'instant il s'agit d'une application non sécurisé et constitue dans un premier temps un projet
personnel.

Pour faire tourner l'API il convient d'avoir des données en faisant tourner le script script/getData/scriptGetDataOneShot.R

