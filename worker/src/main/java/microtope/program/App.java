package microtope.program;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Arrays;

import javax.jms.JMSException;

import microtope.config.ActiveMqConfiguration;
import microtope.config.SqlConfig;
import microtope.worker.ActiveMqMessageReciever;
import microtope.worker.DatabaseInsertListener;
import microtope.worker.MariaDbWriter;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class App {
	
	private static Logger logger = LogManager.getLogger(App.class);

    public static void main(String[] args) throws IOException, InterruptedException, SQLException, JMSException {
        logger.info("Starting Worker");
        
        ActiveMqConfiguration amqconf = ActiveMqConfiguration.emptyConfig();
        SqlConfig sqlconf = SqlConfig.emptyConfig();
        
        if (args.length != 10) {
        	logger.error("The args have to be: ActiveMQ_IP ActiveMQ_Port ActiveMQ_Queue ActiveMQ_User ActiveMQ_Pwd");
        	throw new IllegalArgumentException("Did not get enough args!");
        }  else {
        	String[] amqargs = Arrays.copyOfRange(args, 0,5);
        	amqconf = ActiveMqConfiguration.createActiveMqConfigFromArgs(amqargs);
        	String[] sqlargs = Arrays.copyOfRange(args, 5, 10);
        	sqlconf = SqlConfig.createSqlConfigFromArgs(sqlargs);
        	
            logger.info("args[] are ok, starting worker ...");

            ActiveMqMessageReciever reciever = new ActiveMqMessageReciever(amqconf);
			reciever.open(reciever.createConnectionFromConfig());
            
            var mariadbwriter = new MariaDbWriter(sqlconf);
            mariadbwriter.open(mariadbwriter.buildConnectionFromConfig());
            var listener = new DatabaseInsertListener(mariadbwriter);
            
            reciever.registerMessageListener(listener);
        }
    }

}