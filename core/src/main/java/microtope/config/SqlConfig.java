package microtope.config;

import microtope.data.ValueChecker;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SqlConfig {
	private static final Logger logger = LogManager.getLogger(SqlConfig.class);

    public String addressToConnect = null;
    public String portToConnect = null;
    public String databaseToConnect = null;
    public String userToConnect = null;
    public String passwordToConnect = null;
    
    
    // Both Constructors are private to use only the Factory Methods!
    //Empty Constructor to yield default values
    private SqlConfig() {
    	
    }
    
    //Constructor with Args
    private SqlConfig(String address, String port, String database, String user, String pwd) {
    	addressToConnect = address;
    	portToConnect = port;
    	databaseToConnect = database;
    	userToConnect = user;
    	passwordToConnect = pwd;
    }
    
    public static SqlConfig createSqlConfigFromArgs(String[] args) {
        if (args.length != 5) {
        	logger.error("Did not get enough args for SQLConfig!");
        	return new SqlConfig();
        }
        
        var addressToCheck = args[0];
    	var portToCheck = args[1];
    	var queueToCheck = args[2];
    	if (addressToCheck == null || addressToCheck.isEmpty()) {
    		logger.error("Recieved null or empty Adress");
        	return new SqlConfig();
    	}
    	if (portToCheck == null || portToCheck.isEmpty()) {
    		logger.error("Recieved null or empty port");
        	return new SqlConfig();
    	}
    	if (queueToCheck == null || queueToCheck.isEmpty()) {
    		logger.error("Recieved null or empty topic");
        	return new SqlConfig();
    	}
    	if (!ValueChecker.goodUrl(addressToCheck)) {
    		logger.warn(addressToCheck + " does not look like valid IP Adress or Domain Name!");
    		logger.warn("trying to connect to " + addressToCheck + " anyway...");
    	}
    	if (!ValueChecker.goodPort(portToCheck)) {
    		logger.error(portToCheck + " is not a valid Port!");
    		return new SqlConfig();
        }
    	//TODO: ValueChecker for Queue-Names?
    	logger.info("args[] are ok, starting sender ...");
    	
    	return new SqlConfig(addressToCheck,portToCheck,queueToCheck,args[3],args[4]);
    }
    
    public static SqlConfig emptyConfig() {
    	return new SqlConfig();
    }
    
    public boolean isEmpty() {
    	   return addressToConnect == null 
    			   && portToConnect == null 
    			   && databaseToConnect == null;
   }
    
}
