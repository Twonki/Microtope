package microtope.config;

import microtope.data.ValueChecker;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ActiveMqConfiguration {
	private static Logger logger = LogManager.getLogger(ActiveMqConfiguration.class);

    public String addressToConnect = null;
    public String portToConnect = null;
    public String queueToConnect = null;
    public String userToConnect = null;
    public String passwordToConnect = null;
    
    
    // Both Constructors are private to use the Factory Methods!
    
    //Empty Constructor to yield default values
    private ActiveMqConfiguration() {
    	
    }
    
    //Constructor with Args
    private ActiveMqConfiguration(String address, String port, String queue, String user, String pwd) {
    	addressToConnect = address;
    	portToConnect = port;
    	queueToConnect = queue;
    	userToConnect = user;
    	passwordToConnect = pwd;
    }
    
    public static ActiveMqConfiguration createActiveMqConfigFromArgs(String[] args) {
        if (args.length != 5) {
        	logger.error("Did not get enough args!");
        	logger.error("The args have to be: ActiveMQ_IP ActiveMQ_Port ActiveMQ_Queue ActiveMQ_User ActiveMQ_Pwd");
        	return new ActiveMqConfiguration();
        }
        
        var addressToConnect = args[0];
    	var portToConnect = args[1];
    	var queueToConnect = args[2];
    	if (addressToConnect == null || addressToConnect.isEmpty()) {
    		logger.error("Recieved null or empty Adress");
        	return new ActiveMqConfiguration();
    	}
    	if (portToConnect == null || portToConnect.isEmpty()) {
    		logger.error("Recieved null or empty port");
        	return new ActiveMqConfiguration();
    	}
    	if (queueToConnect == null || queueToConnect.isEmpty()) {
    		logger.error("Recieved null or empty topic");
        	return new ActiveMqConfiguration();
    	}
    	
    	if (!ValueChecker.goodUrl(addressToConnect)) {
    		logger.warn(addressToConnect + " does not look like valid IP Adress or Domain Name!");
    		logger.warn("trying to connect to " + addressToConnect + " anyway...");
    	}
    	if (!ValueChecker.goodPort(portToConnect)) {
    		logger.error(portToConnect + " is not a valid Port!");
    		return new ActiveMqConfiguration();
        }
    	//TODO: ValueChecker for Queue-Names?
    	logger.info("args[] are ok, starting sender ...");
    	
    	return new ActiveMqConfiguration(addressToConnect,portToConnect,queueToConnect,args[3],args[4]);
    }
    
    public static ActiveMqConfiguration emptyConfig() {
    	return new ActiveMqConfiguration();
    }
    
    public boolean isEmpty() {
    	   return addressToConnect == null 
    			   && portToConnect == null 
    			   && queueToConnect == null;
   }
    
}
