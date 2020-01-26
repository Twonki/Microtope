package microtope.worker;

import javax.jms.Message;
import javax.jms.MessageListener;
import microtope.messages.AmqMessage;
import microtope.messages.AmqMessageParser;
import microtope.messages.BadMessage;
import microtope.messages.CoinMessage;
import microtope.messages.LoginMessage;
import microtope.messages.LogoutMessage;
import microtope.messages.StepMessage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DatabaseInsertListener implements MessageListener {

	private static Logger logger = LogManager.getLogger(DatabaseInsertListener.class);
	
	DatabaseWriter writer;
	
	public DatabaseInsertListener(DatabaseWriter writer) {
		if (writer == null) {
			throw new IllegalArgumentException("Writer cannot be null!");	
		}
		
		this.writer = writer;
	}
	
	@Override
	public void onMessage(Message message) {
		AmqMessage msg = AmqMessageParser.parseJmsMessage(message);
		
		if (msg instanceof BadMessage) {
			logger.debug("Recieved bad Message - not doing anything");
		} else if (msg instanceof LoginMessage) {
			LoginMessage msgParsed = (LoginMessage) msg;
			writer.writeLogin(msgParsed);
		} else if (msg instanceof LogoutMessage) {
			LogoutMessage msgParsed = (LogoutMessage) msg;
			writer.writeLogout(msgParsed);
		} else if (msg instanceof StepMessage) {
			StepMessage msgParsed = (StepMessage) msg;
			writer.writeSteps(msgParsed);
		} else if (msg instanceof CoinMessage) {
			CoinMessage msgParsed = (CoinMessage) msg;
			writer.writeCoins(msgParsed);
		} else {
			logger.debug("Recieved Unhandeled Message Class!");
		}
	}

}
