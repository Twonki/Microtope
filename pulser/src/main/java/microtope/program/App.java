package microtope.program;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import javax.jms.JMSException;
import microtope.config.ActiveMqConfiguration;
import microtope.pulser.ActiveMqMessageSender;
import microtope.pulser.Worker;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class App {
	
	private static Logger logger = LogManager.getLogger(App.class);

    public static void main(String[] args) throws JMSException, IOException, InterruptedException, ExecutionException {
        logger.info("Starting AMQ-Sender");
        logger.debug("Recieved " + args + " as arguments");
        ActiveMqConfiguration amqconf = ActiveMqConfiguration.emptyConfig();
        
        amqconf = ActiveMqConfiguration.createActiveMqConfigFromArgs(args);
        
        ActiveMqMessageSender sender = new ActiveMqMessageSender(amqconf);
        
        logger.info("Creating Worker...");
        Worker worker = Worker.randomWorker(sender, 50);
        
        sender.open(sender.createConnectionFromConfig());
        
        ExecutorService executor = Executors.newFixedThreadPool(1);
		Future<?> future = executor.submit(() -> {
			try {
				worker.work();
			} catch (JMSException | InterruptedException e) {
				logger.error(e);
			}
		});
		future.get(); // wait for completion of work
        
        sender.close();
    }
}
