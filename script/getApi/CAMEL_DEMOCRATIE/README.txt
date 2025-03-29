mkdir route_camel
cd route_camel

curl -Ls https://sh.jbang.dev | bash -s - trust add https://github.com/apache/
curl -Ls https://sh.jbang.dev | bash -s - app install --fresh --force camel@apache/camel

-- déployer les fichiers sur le dossier route_camel

camel run post_emargement.java post_vote.java get_vote.java get_emargement.java