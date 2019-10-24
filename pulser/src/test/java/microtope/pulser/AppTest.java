package microtope.pulser;

import static org.junit.jupiter.api.Assertions.assertThrows;

import javax.jms.JMSException;

import org.junit.jupiter.api.Test;

import microtope.program.App;

/**
 * Unit test for simple App.
 */
public class AppTest{
   
	@Test
	void testMain_toLowNumberOfArguments_shouldThrowIllegalArgumentException() {
		String[] testArgs= new String[] {"Adress","1005","Queue"};
		
		assertThrows(IllegalArgumentException.class, () -> App.main(testArgs));
	}
	
	@Test
	void testMain_toHighNumberOfArguments_shouldThrowIllegalArgumentException() {
		String[] testArgs= new String[] {"Adress","1005","Queue","User","Pwd","One to high"};
		
		assertThrows(IllegalArgumentException.class, () -> App.main(testArgs));
	}
	
	@Test
	void testMain_goodArguments_butOffline_shouldThrowJMSException() {
		String[] testArgs= new String[] {"Adress","1005","Queue","User","Pwd"};
		
		assertThrows(JMSException.class, () -> App.main(testArgs));
	}
}
