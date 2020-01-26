package microtope.messages;

import java.util.regex.Pattern;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class AmqMessageParser {
	private static Logger logger = LogManager.getLogger(AmqMessageParser.class);
	
	private static final Pattern loginPattern = Pattern.compile("E: Player (\\d+) logged in for team (\\d+)");
	private static final Pattern logoutPattern = Pattern.compile("E: Player (\\d+) logged out");
	private static final Pattern coinPattern = Pattern.compile("E: Player (\\d+) collected (\\d+) coins for Team (\\d+)");
	private static final Pattern stepPattern = Pattern.compile("M: Player (\\d+) moved (\\d+) steps");
	
	public static AmqMessage parseTextMessage(String msg) {
		logger.trace("parsing message " + msg);
		
		if (!msg.startsWith("M: ") && !msg.startsWith("E: ")) {
			logger.debug("Recieved a Message with strange format");
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
