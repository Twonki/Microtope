package microtope.worker;

import java.io.Closeable;
import java.io.IOException;
import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.Session;
import microtope.config.ActiveMqConfiguration;
import org.apache.activemq.ActiveMQConnectionFactory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ActiveMqMessageReciever implements Closeable, MessageReciever {
	
	private static Logger logger = LogManager.getLogger(ActiveMqMessageReciever.class);

	Connection connection; 
	Session session;
	
	private ActiveMqConfiguration amqConfig;
	private MessageConsumer consumer;

    public ActiveMqMessageReciever(ActiveMqConfiguration amqconf) {
        if (amqconf.isEmpty()) {
        	throw new IllegalArgumentException("AMQConf is Empty!");	
        }
        amqConfig = amqconf;        
    }

	public void open(Connection connection) throws IOException, JMSException {
		this.connection = connection;
        connection.start();
        
        //Creating a non transactional session to send/receive JMS message.
        session = connection.createSession(false,
                Session.AUTO_ACKNOWLEDGE);
        
        //Destination represents here our queue on the AMQ-Server. 
        //The queue will be created automatically on the server.
        Destination destination = session.createQueue(amqConfig.queueToConnect); 
        
        // MessageConsumer is used for receiving (consuming) messages
        consumer = session.createConsumer(destination);
        
        logger.debug("Created fully working AMQ MessageReciever");
        logger.info("Opened the sender successfully - connection, session and producer are running");       
	}
	
	@Override
	public void close() throws IOException {
		try {
			session.close();
			connection.close();
			logger.debug("Closing AMQMessageReciever");
		} catch (JMSException e) {
			logger.error(e);
		} catch (NullPointerException ne) {
			logger.warn("Tryed closing MessasgeReceiver - but was never opened!");
		}
	}

	
	public Connection createConnectionFromConfig() throws JMSException {
		var url = String.format("tcp://%s:%s", this.amqConfig.addressToConnect, amqConfig.portToConnect);
		logger.info("Sender connecting to " + url);
		
        // Getting JMS connection from the server
        ActiveMQConnectionFactory connectionFactory = new ActiveMQConnectionFactory(url);
        
        return connectionFactory.createConnection(amqConfig.userToConnect,amqConfig.passwordToConnect);
	}
	
	@Override
	public void registerMessageListener(MessageListener msglst) {
		if (consumer != null) {
			try {
				consumer.setMessageListener(msglst);
				logger.info("registered new MessageListener of type " + msglst.getClass().getSimpleName());
			} catch (JMSException e) {
				logger.error(e);
			}
		}
	}   
    
}
