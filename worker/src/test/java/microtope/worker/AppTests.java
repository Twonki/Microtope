package microtope.worker;

import static org.junit.jupiter.api.Assertions.*;

import javax.jms.JMSException;

import org.junit.jupiter.api.Test;

import microtope.program.App;

class AppTests {

	@Test
	void testMain_tooLowArguments_shouldThrowIllegalArgumentException() {
		String[] testArgs = {"OnlyOne"};
		assertThrows(IllegalArgumentException.class, () -> {App.main(testArgs);});
	}
	
	@Test
	void testMain_tooManyArguments_shouldThrowIllegalArgumentException() {
		String[] testArgs = {"Too Much","Too Much","Too Much","Too Much","Too Much","Too Much","Too Much","Too Much","Too Much","Too Much","Too Much","Too Much","Too Much","Too Much","Too Much","Too Much"};
		assertThrows(IllegalArgumentException.class, () -> {App.main(testArgs);});
	}
	
	@Test
	void testMain_has10Arguments_ButIsOffline_shouldThrowJMSException() {
		// I have the right number of Arguments - but there is no message queue
		String[] testArgs = {
				"AMQAdress","1006","Queue","AmqUser","AmqPwd",
				"DBAdress","1005","Database","DBUser","DBPwd"};
		assertThrows(JMSException.class, () -> {App.main(testArgs);});
	}
}
