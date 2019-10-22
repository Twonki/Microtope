package microtope.messages;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import org.junit.jupiter.api.Test;

public class MessageHashCodeTests {
	
	@Test
	void testCoinMessage_sameValues_shouldBeTheSame() {
		AmqMessage first = new CoinMessage(30705,1);
		
		AmqMessage other = new CoinMessage(30705,1);
		
		assertEquals(first.hashCode(),other.hashCode());
	}
	
	@Test
	void testCoinMessage_differentPlayer_shouldNotBeTheSame() {
		AmqMessage first = new CoinMessage(30705,1);
		
		AmqMessage other = new CoinMessage(30702,1);
		
		assertNotEquals(first.hashCode(),other.hashCode());
	}
	
	@Test
	void testStepMessage_sameValues_shouldBeTheSame() {
		AmqMessage first = new StepMessage(30705,10);
		
		AmqMessage other = new StepMessage(30705,10);
		
		assertEquals(first.hashCode(),other.hashCode());
	}
	
	@Test
	void testEquals_twoBadMessages_shouldBeTheSame() {
		var bad1 = new BadMessage();
		var bad2 = new BadMessage();
		
		assertEquals(bad1.hashCode(),bad2.hashCode());
	}

	@Test
	void testEquals_compareLogoutMessageToOtherLogoutMessage_shouldBeNotTheSame() {
		AmqMessage first = new LogoutMessage(30705);
		AmqMessage second = new LogoutMessage(30710);
		
		assertNotEquals(first.hashCode(),second.hashCode());
	}
	

	@Test
	void testEquals_compareLoginMessageToLoginMessageWithOtherPlayer_shouldNotBeTheSame() {
		AmqMessage first = new LoginMessage(30705,10);
		AmqMessage second = new LoginMessage(30710,10);
		
		assertNotEquals(first.hashCode(),second.hashCode());
	}
	@Test
	void testEquals_compareLoginMessageToLoginMessageWithSamePlayer_ButWithOtherTeam_shouldNotBeTheSame() {

		AmqMessage first = new LoginMessage(30705,10);
		AmqMessage second = new LoginMessage(30705,11);
		
		assertNotEquals(first.hashCode(),second.hashCode());
	}
}
