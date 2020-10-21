package microtope.config;

import microtope.data.ValueChecker;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Map;

public class ActiveMqConfiguration {
	private static final Logger logger = LogManager.getLogger(ActiveMqConfiguration.class);

    public final String addressToConnect;
    public final String portToConnect;
    public final String queueToConnect;
    public final String userToConnect;
    public final String passwordToConnect;

    /**
     * The primary constructor of ActiveMQ Configuration.
     * Is set to private, to use the public factory methods instead.
     * Value-Checking is done in the factory methods as well.
     * @param address The Address of ActiveMQ. Can Either be a domain-name, url or ip address
     * @param port The port to connect given as a String. Must be a valid port though.
     * @param queue The Queue/Topic to connect to.
     * @param user Optional: The User to connect to the database.
     * @param pwd Optional: The Password to connect to the database.
     */
    private ActiveMqConfiguration(String address, String port, String queue, String user, String pwd) {
    	addressToConnect = address;
    	portToConnect = port;
    	queueToConnect = queue;
    	userToConnect = user;
    	passwordToConnect = pwd;
    }

    /**
     * Builds the ActiveMQConfiguration from System-Args.
     * The Arguments need to be sliced to be precisely the ones required.
     * If you have e.g. attributes for both ActiveMQ and SQL, they need to be (accurately) separated.
     * If there are issues with the values, an empty configuration is returned and an error is logged.
     * In case you are facing issues with the applications configuration, look into the log-files.
     *
     * @param args - the sliced system args provided to the main application. Should only (and exactly) contain the required
     * @return the build activeMQConfiguration or an empty configuration if there are issues with the values.
     */
    public static ActiveMqConfiguration createActiveMqConfigFromArgs(String[] args) {
        if (args.length != 5) {
        	logger.error("Did not get enough args!");
        	logger.error("The args have to be: ActiveMQ_IP ActiveMQ_Port ActiveMQ_Queue ActiveMQ_User ActiveMQ_Pwd");
        	return emptyConfig();
        }
        
        var addressToConnect = args[0];
    	var portToConnect = args[1];
    	var queueToConnect = args[2];
    	if (addressToConnect == null || addressToConnect.isEmpty()) {
    		logger.error("Received null or empty Address");
        	return emptyConfig();
    	}
    	if (portToConnect == null || portToConnect.isEmpty()) {
    		logger.error("Received null or empty port");
        	return emptyConfig();
    	}
    	if (queueToConnect == null || queueToConnect.isEmpty()) {
    		logger.error("Received null or empty topic");
        	return emptyConfig();
    	}
    	
    	if (!ValueChecker.goodUrl(addressToConnect)) {
    		logger.warn(addressToConnect + " does not look like valid IP Address or Domain Name!");
    		logger.warn("trying to connect to " + addressToConnect + " anyway...");
    	}
    	if (!ValueChecker.goodPort(portToConnect)) {
    		logger.error(portToConnect + " is not a valid Port!");
    		return emptyConfig();
        }
    	//TODO: ValueChecker for Queue-Names?
    	logger.info("args[] are ok, starting sender ...");
    	
    	return new ActiveMqConfiguration(addressToConnect,portToConnect,queueToConnect,args[3],args[4]);
    }

    /**
     * Factory Methods to build the ActiveMQ-Configuration from a Map of Attributes.
     * It is intended to be used for e.g. configurations read from a .properties file.
     * To fail gracefully, in case of an missing configuration a empty configuration is returned and an error is logged.
     * </p>
     * For a reference on usage and expected behaviour, refer to the tests.
     *
     * @param properties - the properties read/supplied from a configuration. It is
     * @return the build ActiveMQConfiguration-Object, returns an empty configuration on invalid / missing properties.
     */
    public static ActiveMqConfiguration createConfigFromProperties(Map<String,String> properties) {
        String address;
        String port;
        String queue;
        String user = "Anonymus";
        String password = null;
        if (properties.containsKey("ActiveMqAddress") && !properties.get("ActiveMqAddress").isEmpty()) {
            address = properties.get("ActiveMqAddress");
        } else {
            logger.error("Received null or empty address");
            return emptyConfig();
        }
        if (properties.containsKey("ActiveMqPort") && !properties.get("ActiveMqPort").isEmpty()) {
            port = properties.get("ActiveMqPort");
        } else {
            logger.error("Received null or empty Port");
            return emptyConfig();
        }
        if (properties.containsKey("ActiveMqQueue") && !properties.get("ActiveMqQueue").isEmpty()) {
            queue = properties.get("ActiveMqQueue");
        } else {
            logger.error("Received null or empty topic");
            return emptyConfig();
        }
        // For User and Password, it does not fail on an missing entry
        // The password is only looked for if there is a user (would not make sense otherwise)
        if (properties.containsKey("ActiveMqUser")) {
            user = properties.get("ActiveMqUser");
            if (properties.containsKey("ActiveMqPassword")) {
                password = properties.get("ActiveMqPassword");
            } else {
                logger.warn("Received a user but no password.");
            }
        } else {
            logger.warn("Received null or empty User, trying to connect anyway as '" + user + "'.");
        }

        return new ActiveMqConfiguration(address,port,queue,user,password);
    }

    /**
     * Constructor for the Empty Configuration.
     * Used for failing activeMQConfiguration and for Mocks/Stubs.
     * @return a Configuration with all values initialized to "null"
     */
    public static ActiveMqConfiguration emptyConfig() {
    	return new ActiveMqConfiguration(null,null,null,null,null);
    }

    /**
     * Simple method to check if the values are initialized.
     * Used for easy error-finding in the using-classes.
     * The Constructor using ActiveMQ-Configurations e.g. checks if the supplied Configuration is empty.
     * @return a boolean whether all of the (always) required attributes are initialized
     */
    public boolean isEmpty() {
    	   return addressToConnect   == null
    			   && portToConnect  == null
    			   && queueToConnect == null;
   }
    
}
