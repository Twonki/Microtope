package microtope.data;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class MessageGeneratorTest {

	@Test
	void testCreateLoginMessage_playerIs10_shouldContain10() {
		var mes = MessageGenerator.createLoginMessage(10, 5);
		
		assertTrue(mes.contains("10"));
	}

	
	@Test
	void testCreateLogoutMessage_playerIs10_shouldContain10() {
		var mes = MessageGenerator.createLogoutMessage(10);
		
		assertTrue(mes.contains("10"));
	}

	@Test
	void testCreateCoinMessage_coinsAre3_shouldContain3() {
		var mes = MessageGenerator.createCoinMessage(10,5, 3);
		
		assertTrue(mes.contains("3"));
	}
	@Test
	void testCreateCoinMessage_teamHasNoName_shouldRunThrough() {
		var mes = MessageGenerator.createCoinMessage(10,50501, 3);
		return;
	}
	
	@Test
	void testCreateCoinMessage_playerIs10_shouldContain10() {
		var mes = MessageGenerator.createCoinMessage(10,5, 3);
		
		assertTrue(mes.contains("10"));
	}
	
	@Test
	void testCreateStepMessage_stepsAre7_shouldContain7() {
		var mes = MessageGenerator.createStepMessage(10, 7);
		
		assertTrue(mes.contains("7"));
	}
	
	@Test
	void testCreateStepMessage_PlayerIs10_shouldContain10() {
		var mes = MessageGenerator.createStepMessage(10, 7);
		
		assertTrue(mes.contains("10"));
	}
}
