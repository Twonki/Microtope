package microtope.messages;

import java.util.Date;
import java.util.regex.Pattern;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.TextMessage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * This class contains methods to parse incoming JMS-Messages (the class received from AMQ or other message queues)
 * into the self-given Message Format for our java logic.
 * Every parse-method has a corresponding regex in the toplevel declarations of fields,
 * and every error-case in parsing returns a BadMessage-Object.
 *
 * Be careful with changing the patterns, and always look into the sister-class MessageGenerator.java
 */
public final class AmqMessageParser {
	private static final Logger logger = LogManager.getLogger(AmqMessageParser.class);
	
	private static final Pattern loginPattern = Pattern.compile("E: Player (\\d+) logged in for team (\\d+)");
	private static final Pattern logoutPattern = Pattern.compile("E: Player (\\d+) logged out");
	private static final Pattern coinPattern = Pattern.compile("E: Player (\\d+) collected (\\d+) coins for Team (\\d+)");
	private static final Pattern stepPattern = Pattern.compile("M: Player (\\d+) moved (\\d+) steps");

    /**
     * Tries to parse a given JMS Message into one of the own-message formats.
     * If there is either an error in parsing (e.g. the message format) or the content (e.g. the text of the message)
     * a BadMessage is returned.
     *
     * All Errors are caught in this method and BadMessages are returned and errors are logged.
     *
     * @param message a message read from an AMQ-Queue
     * @return the parsed message of a corresponding core-class or a bad message.
     */
	public static AmqMessage parseJmsMessage(Message message) {
		try {
			if (message instanceof TextMessage) {
	            TextMessage textMessage = (TextMessage) message;
	            AmqMessage toReturn = AmqMessageParser.parseTextMessage(textMessage.getText());
	            toReturn.setTimeStamp(new Date(message.getJMSTimestamp()));
	            return toReturn;
	        } else {
				logger.warn("Received Message but it is no TextMessage");
				return new BadMessage();
			}
		} catch (JMSException e) {
			logger.error(e);
			return new BadMessage();
		}
	}

    /**
     * This method applies the defined regex-patterns to create an AMQ message of the String.
     * The patterns are applied in order, and if no pattern matches a bad message is returned.
     *
     * The timestamp is set separately as it requires information of the JMS-Message and is done in parseJmsMessage.
     *
     * It is intentionally separated from parseJmsMessage,
     * so they cleaner operate on different levels of granularity and responsibility.
     * Also, it is much easier to test from the String messages.
     *
     * @param msg the text message part of a given
     * @return the first matched pattern or a bad message.
     */
	public static AmqMessage parseTextMessage(String msg) {
		logger.trace("parsing message " + msg);

		if (msg == null) {
		    logger.debug("Received a null-message");
		    return new BadMessage();
        }
		if (!msg.startsWith("M: ") && !msg.startsWith("E: ")) {
			logger.debug("Received a Message with unrecognized prefix");
			return new BadMessage();
		}
	
		var loginMatcher = loginPattern.matcher(msg);
		if (loginMatcher.matches()) {
			int playerId = Integer.parseInt(loginMatcher.group(1));
			int teamId = Integer.parseInt(loginMatcher.group(2));
			return new LoginMessage(playerId,teamId);
		}
		
		var logoutMatcher = logoutPattern.matcher(msg);
		if (logoutMatcher.matches()) {
			int playerId = Integer.parseInt(logoutMatcher.group(1));
			return new LogoutMessage(playerId);
		}
		
		var stepMatcher = stepPattern.matcher(msg);
		if (stepMatcher.matches()) {
			int playerId = Integer.parseInt(stepMatcher.group(1));
			int steps = Integer.parseInt(stepMatcher.group(2));
			return new StepMessage(playerId,steps);
		}
		
		var coinMatcher = coinPattern.matcher(msg);
		if (coinMatcher.matches()) {
			int playerId = Integer.parseInt(coinMatcher.group(1));
			int coins = Integer.parseInt(coinMatcher.group(2));
			return new CoinMessage(playerId,coins);
		}
		
		return new BadMessage();
	}
		
}
