sudo -u postgres psql

create database bdd_democratie;
create extension postgis;

\c bdd_democratie;


sudo apt install postgis postgresql-postgis
sudo apt install postgresql postgresql-contrib