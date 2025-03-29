// camel-k: language=java
// camel-k: dependency=org.postgresql:postgresql:42.2.23

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;
import org.apache.commons.dbcp2.BasicDataSource;

import org.apache.camel.builder.RouteBuilder;


public class get_emargement extends RouteBuilder {

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
		
		BasicDataSource dataSource = new BasicDataSource();
        dataSource.setDriverClassName("org.postgresql.Driver");
        dataSource.setUrl("jdbc:postgresql://" + serverName +":5432/" + databaseName);
        dataSource.setUsername(username);
        dataSource.setPassword(password);
		
	    getContext().getRegistry().bind("myDataSource", dataSource);


        from("jetty://http://0.0.0.0:1400/assemblee_nationale/get_emargement")
		        .setBody(constant("SELECT mail, nom, prenom, departement, naissance, parti, identifiant, date FROM assemblee_representative.emargement"))
				.to("jdbc:myDataSource");
    }
}
