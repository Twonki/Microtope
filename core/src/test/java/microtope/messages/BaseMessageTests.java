package microtope.messages;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Date;

import org.junit.jupiter.api.Test;

class BaseMessageTests {

	@Test
	void testGetTimeStamp_nothingSetYet_shouldBeZeroDate() {
		AmqMessage testObject = new StepMessage(30705,10);
		
		Date zeroDate = new Date(0);
		
		assertEquals(zeroDate,testObject.getTimeStamp());
	}

	
	@Test
	void testSetTimeStamp_shouldBeNewDate() {
		AmqMessage testObject = new StepMessage(30705,10);
		Date expectedDate = new Date(10);
		Date toSet= new Date(10);
		
		testObject.setTimeStamp(toSet);
		
		assertEquals(expectedDate,testObject.getTimeStamp());
	}

    @Test
    void testSetTimeStamp_OnBadMessage_shouldNeverBeSet() {
        AmqMessage testObject = new BadMessage();
        Date toSet= new Date(10);

        testObject.setTimeStamp(toSet);

        assertNull(testObject.getTimeStamp());
    }

	@Test
	void testGetTimeStamp_ofBadMessage_shouldBeNull() {
		AmqMessage bad = new BadMessage();
		
		assertNull(bad.getTimeStamp());
	}
}
