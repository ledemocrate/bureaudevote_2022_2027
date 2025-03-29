import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

import org.apache.camel.builder.RouteBuilder;


public class post_emargement extends RouteBuilder {

    @Override
    public void configure() throws Exception {
        Properties properties = new Properties();
        try (FileInputStream fis = new FileInputStream("database.properties")) {
            properties.load(fis);
        } catch (IOException e) {
            e.printStackTrace();
            // Gérer l'exception de chargement du fichier de propriétés
        }

        String databaseName = properties.getProperty("postgresql.databaseName");
        String serverPort = properties.getProperty("postgresql.serverPort");
        String serverName = properties.getProperty("postgresql.serverName");
        String password = properties.getProperty("postgresql.password");
        String username = properties.getProperty("postgresql.username");

        from("jetty://http://0.0.0.0:1400/assemblee_nationale/post_emargement")
                .to("kamelet:postgresql-sink?" +
                        "databaseName=" + databaseName +
                        "&serverPort=" + serverPort +
                        "&serverName=" + serverName +
                        "&password=" + password +
                        "&username=" + username +
                        "&query=INSERT INTO assemblee_representative.emargement (mail,nom ,prenom ,departement,naissance,parti,identifiant ,date ) VALUES (:#mail,:#nom,:#prenom,:#departement,CAST(:#naissance AS DATE),:#parti,:#identifiant,CURRENT_TIMESTAMP)");
    }
}	