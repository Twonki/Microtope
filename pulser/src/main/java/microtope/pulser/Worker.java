package microtope.pulser;

import javax.jms.JMSException;
import microtope.data.DataGenerator;
import microtope.data.MessageGenerator;
import microtope.data.Team;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Worker {
	
	private static Logger logger = LogManager.getLogger(Worker.class);

	private final int playerId;
	private final int teamId;
	MessageSender sender;
	
	private int numberOfMessages = 0;
	
	private int timeout = 100;
	private double coinQuota = 0.1; //Every 10th Event will be Coin
	
	public boolean finished = false;
	
	public Worker(int playerId, int teamId, MessageSender sender, int numberOfMessages) {
		
		this.playerId = playerId;
		this.sender = sender;
		this.numberOfMessages = numberOfMessages;
		this.teamId = teamId;
		
		logger.debug("Worker created with Team " + teamId + " and playerId " + playerId);
	}
	
	public void work() throws JMSException, InterruptedException {
		logger.debug("Worker starts work with " + numberOfMessages + " messages to send");
		
		synchronized (this) {
			sender.sendMessage(MessageGenerator.createLoginMessage(playerId,teamId));
			for (int i = 0;i < numberOfMessages;i++) {
				this.wait(timeout);
				sendRandomizedMessage();
			}
			logger.debug("Worker send his messages");
			sender.sendMessage(MessageGenerator.createLogoutMessage(playerId));
			finished = true;
			logger.trace("Worker finished");
			
			notify();
		}
	}
	
	public void sendRandomizedMessage() {
		try {
			String message;
			if (Math.random() < coinQuota) {
				message = MessageGenerator.createCoinMessage(playerId, teamId, DataGenerator.getRandomCoins());	
			} else {
				message = MessageGenerator.createStepMessage(playerId, DataGenerator.getRandomSteps());	
			}
			
			sender.sendMessage(message);
		} catch (JMSException e) {
			logger.error(e);
		}
	}
	
	public static Worker randomWorker(MessageSender sender, int messages) {
		Team t = DataGenerator.getRandomTeam();
		int playerId = DataGenerator.getRandomPlayerNumber();
		Worker w = new Worker(playerId,t.ordinal(), sender, messages);
		return w;
	}
	
	
}
