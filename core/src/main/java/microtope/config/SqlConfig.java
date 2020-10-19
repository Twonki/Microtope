package microtope.config;

import microtope.data.ValueChecker;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Map;

public class SqlConfig {
	private static final Logger logger = LogManager.getLogger(SqlConfig.class);

    public final String addressToConnect;
    public final String portToConnect;
    public final String databaseToConnect;
    public final String userToConnect;
    public final String passwordToConnect;

    /**
     * Constructor for building SQLConfigurations
     * </p>
     * private constructor - only use the factory methods.
     * Value-Checking is done in the regarding factory methods.
     *
     * @param address of the database to connect to. Can be an IP or a domainname (localhost)
     * @param port port to connect to, must be a valid port (Number)
     * @param database database to connect to. Cannot be null.
     * @param user optional
     * @param pwd optional (only used when user is given)
     */
    private SqlConfig(String address, String port, String database, String user, String pwd) {
    	addressToConnect = address;
    	portToConnect = port;
    	databaseToConnect = database;
    	userToConnect = user;
    	passwordToConnect = pwd;
    }

    /**
     * Builds the ActiveMQConfiguration from System-Args.
     * The Arguments need to be sliced to be precisely the ones required.
     * If you have e.g. attributes for both SQL and ActiveMQ, they need to be (accurately) separated.
     * If there are issues with the values, an empty configuration is returned and an error is logged.
     * In case you are facing issues with the applications configuration, look into the log-files.
     *
     * @param args - the sliced system args provided to the main application. Should only (and exactly) contain the required
     * @return the build SqlConfig or an empty configuration if there are issues with the values.
     */
    public static SqlConfig createSqlConfigFromArgs(String[] args) {
        if (args.length != 5) {
        	logger.error("Did not get enough args for SQLConfig!");
        	return emptyConfig();
        }
        
        var addressToCheck = args[0];
    	var portToCheck = args[1];
    	var queueToCheck = args[2];
    	if (addressToCheck == null || addressToCheck.isEmpty()) {
    		logger.error("Received null or empty Adress");
        	return emptyConfig();
    	}
    	if (portToCheck == null || portToCheck.isEmpty()) {
    		logger.error("Received null or empty port");
            return emptyConfig();
    	}
    	if (queueToCheck == null || queueToCheck.isEmpty()) {
    		logger.error("Received null or empty topic");
        	return emptyConfig();
    	}
    	if (!ValueChecker.goodUrl(addressToCheck)) {
    		logger.warn(addressToCheck + " does not look like valid IP Address or Domain Name!");
    		logger.warn("trying to connect to " + addressToCheck + " anyway...");
    	}
    	if (!ValueChecker.goodPort(portToCheck)) {
    		logger.error(portToCheck + " is not a valid Port!");
    		return emptyConfig();
        }
    	//TODO: ValueChecker for Queue-Names?
    	logger.info("args[] are ok, starting sender ...");
    	
    	return new SqlConfig(addressToCheck,portToCheck,queueToCheck,args[3],args[4]);
    }


    /**
     * Factory Methods to build the SQLConfiguration from a Map of Attributes.
     * It is intended to be used for e.g. configurations read from a .properties file.
     * To fail gracefully, in case of an missing configuration a empty configuration is returned and an error is logged.
     * </p>
     * For a reference on usage and expected behaviour, refer to the tests.
     *
     * @param properties - the properties read/supplied from a configuration.
     * @return the build SqlConfig-Object, returns an empty configuration on invalid / missing properties.
     */
    public static SqlConfig createConfigFromProperties(Map<String,String> properties) {
        String address, port, queue, user="Anonymus",password = null;
        if (properties.containsKey("SqlAddress")&& !properties.get("SqlAddress").isEmpty()) {
            address = properties.get("SqlAddress");
        } else {
            logger.error("Received null or empty SqlAddress");
            return emptyConfig();
        }
        if (properties.containsKey("SqlPort") && !properties.get("SqlPort").isEmpty()) {
            port = properties.get("SqlPort");
        } else {
            logger.error("Received null or empty SqlPort");
            return emptyConfig();
        }
        if (properties.containsKey("SqlDatabase") && !properties.get("SqlDatabase").isEmpty()) {
            queue = properties.get("SqlDatabase");
        } else {
            logger.error("Received null or empty SqlDatabase");
            return emptyConfig();
        }
        // For User and Password, it does not fail on an missing entry
        // The password is only looked for if there is a user (would not make sense otherwise)
        if (properties.containsKey("SqlUser")){
            user = properties.get("SqlUser");
            if (properties.containsKey("SqlPassword")){
                password = properties.get("SqlPassword");
            } else {
                logger.warn("Received a SqlUser but no password.");
            }
        } else {
            logger.warn("Received null or empty SqlUser, trying to connect anyway as '" + user + "'.");
        }

        return new SqlConfig(address,port,queue,user,password);
    }

    /**
     * Factory for empty configurations (All Values null).
     * Used for gracefully failing other factories and for tests.
     * @return a minimal object with all values set to null.
     */
    public static SqlConfig emptyConfig() {
    	return new SqlConfig(null, null,null,null,null);
    }
    
    public boolean isEmpty() {
    	   return addressToConnect      == null
    			   && portToConnect     == null
    			   && databaseToConnect == null;
   }
    
}
