
-- Drop table

-- DROP TABLE hotel_datamart.dim_bedroom;

CREATE TABLE assemblee_elective.groupe_politique (
	"groupeAbrev" varchar, 
	"Flag" bytea
);

create or replace function bytea_import(p_path text, p_result out bytea) 
                   language plpgsql as $$
declare
  l_oid oid;
begin
  select lo_import(p_path) into l_oid;
  select lo_get(l_oid) INTO p_result;
  perform lo_unlink(l_oid);
end;$$;

insert into assemblee_elective.groupe_politique values ('RN', pg_read_file('C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/script/getData/getImage/quentingollentz.png')::bytea);
insert into assemblee_elective.groupe_politique values ('RE', pg_read_file('C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/script/getData/getImage/quentingollentz.png')::bytea);
insert into assemblee_elective.groupe_politique values ('ECOLO', pg_read_file('C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/script/getData/getImage/quentingollentz.png')::bytea);
insert into assemblee_elective.groupe_politique values ('SOC', pg_read_file('C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/script/getData/getImage/quentingollentz.png')::bytea);
insert into assemblee_elective.groupe_politique values ('LFI-NUPES', pg_read_file('C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/script/getData/getImage/quentingollentz.png')::bytea);
insert into assemblee_elective.groupe_politique values ('LR', pg_read_file('C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/script/getData/getImage/quentingollentz.png')::bytea);
insert into assemblee_elective.groupe_politique values ('DEM', pg_read_file('C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/script/getData/getImage/quentingollentz.png')::bytea);
insert into assemblee_elective.groupe_politique values ('GDR-NUPES', pg_read_file('C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/script/getData/getImage/quentingollentz.png')::bytea);
insert into assemblee_elective.groupe_politique values ('LIOT', pg_read_file('C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/script/getData/getImage/quentingollentz.png')::bytea);
insert into assemblee_elective.groupe_politique values ('HOR', pg_read_file('C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/script/getData/getImage/quentingollentz.png')::bytea);
insert into assemblee_elective.groupe_politique values ('LP', pg_read_file('C:/Users/Quentin GOLLENTZ/OneDrive/Documents/PROJET PERSO/bureaudevote_2022_2027/script/getData/getImage/Les_Patriotes_2018.png')::bytea);

select * from work_ht.image_table