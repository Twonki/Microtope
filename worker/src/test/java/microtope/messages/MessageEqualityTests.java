package microtope.messages;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import org.junit.jupiter.api.Test;

public class MessageEqualityTests {
	
	@Test
	void testCoinMessage_sameValues_shouldBeTheSame() {
		AMQMessage first = new CoinMessage(30705,1);
		
		AMQMessage other = new CoinMessage(30705,1);
		
		assertEquals(first,other);
	}
	
	@Test
	void testCoinMessage_differentPlayer_shouldNotBeTheSame() {
		AMQMessage first = new CoinMessage(30705,1);
		
		AMQMessage other = new CoinMessage(30702,1);
		
		assertNotEquals(first,other);
	}
	
	@Test
	void testCoinMessage_differentCoins_shouldNotBeTheSame() {
		AMQMessage first = new CoinMessage(30705,1);
		
		AMQMessage other = new CoinMessage(30705,2);
		
		assertNotEquals(first,other);
	}
	
	@Test
	void testCoinMessage_sameMessage_shouldBeTheSame() {
		AMQMessage one = new CoinMessage(30705,10);
		
		assertEquals(one,one);
	}
	
	@Test
	void testStepMessage_sameValues_shouldBeTheSame() {
		AMQMessage first = new StepMessage(30705,10);
		
		AMQMessage other = new StepMessage(30705,10);
		
		assertEquals(first,other);
	}
	@Test
	
	void testStepMessage_sameMessage_shouldBeTheSame() {
		AMQMessage one = new StepMessage(30705,10);
		
		assertEquals(one,one);
	}
	
	@Test
	void testStepMessage_differentPlayer_shouldNotBeTheSame() {
		AMQMessage first = new StepMessage(30705,1);
		
		AMQMessage other = new StepMessage(30702,1);
		
		assertNotEquals(first,other);
	}
	
	@Test
	void testStepMessage_differentSteps_shouldNotBeTheSame() {
		AMQMessage first = new StepMessage(30705,10);
		
		AMQMessage other = new StepMessage(30705,22);
		
		assertNotEquals(first,other);
	}
	
	@Test
	void testEquals_twodifferntCoinTypes_shouldNotBeTheSame() {
		AMQMessage first = new StepMessage(30705,10);
		
		AMQMessage other = new CoinMessage(30705,10);
		
		assertNotEquals(first,other);	
	}
	
	@Test
	void testEquals_twoBadMessages_shouldBeTheSame() {
		var bad1 = new BadMessage();
		var bad2 = new BadMessage();
		
		assertEquals(bad1,bad2);
	}

	@Test
	void testEquals_CompareMessageToBadMessage_shouldBeNotTheSame() {
		AMQMessage good = new StepMessage(30705,10);
		var bad = new BadMessage();
		
		assertNotEquals(bad,good);
	}

	@Test
	void testEquals_CompareBadMessageToGoodMessage_shouldBeNotTheSame() {
		AMQMessage good = new StepMessage(30705,10);
		var bad = new BadMessage();
		
		assertNotEquals(good,bad);
	}
	

	@Test
	void testEquals_compareStepMessageToOtherMessageKind_shouldBeNotTheSame() {
		AMQMessage first = new StepMessage(30705,10);
		var bad = new BadMessage();
		
		assertNotEquals(first,bad);
	}

	@Test
	void testEquals_compareCoinMessageToOtherMessageKind_shouldBeNotTheSame() {
		AMQMessage first = new CoinMessage(30705,10);
		var bad = new BadMessage();
		
		assertNotEquals(first,bad);
	}
	
	@Test
	void testEquals_compareLoginMessageToOtherMessageKind_shouldBeNotTheSame() {
		AMQMessage first = new LoginMessage(30705,10);
		var bad = new BadMessage();
		
		assertNotEquals(first,bad);
	}
	
	@Test
	void testEquals_compareLogoutMessageToOtherMessageKind_shouldBeNotTheSame() {
		AMQMessage first = new LogoutMessage(30705);
		var bad = new BadMessage();
		
		assertNotEquals(first,bad);
	}
	

	@Test
	void testEquals_compareLogoutMessageToOtherLogoutMessage_shouldBeNotTheSame() {
		AMQMessage first = new LogoutMessage(30705);
		AMQMessage second = new LogoutMessage(30710);
		
		assertNotEquals(first,second);
	}
	

	@Test
	void testEquals_compareLoginMessageToLoginMessageWithOtherPlayer_shouldNotBeTheSame() {
		AMQMessage first = new LoginMessage(30705,10);
		AMQMessage second = new LoginMessage(30710,10);
		
		assertNotEquals(first,second);
	}
	@Test
	void testEquals_compareLoginMessageToLoginMessageWithSamePlayer_ButWithOtherTeam_shouldNotBeTheSame() {

		AMQMessage first = new LoginMessage(30705,10);
		AMQMessage second = new LoginMessage(30705,11);
		
		assertNotEquals(first,second);
	}
}
