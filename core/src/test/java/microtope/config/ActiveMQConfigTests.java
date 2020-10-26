package microtope.config;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

class ActiveMQConfigTests {

	@Test
	void testConstructor_tooLittleNumberOfArguments_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Address","1005","Queue"};
		
		ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_tooHighNumberOfArguments_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Address","1005","Queue","User","Pwd","One Argument too much!"};
		
		ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyAddress_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"","1005","Queue","User","Pwd"};
		
		ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_nullAddress_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {null,"1005","Queue","User","Pwd"};
		
		ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyPort_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Address","","Queue","User","Pwd"};
		
		ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_nullPort_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Address",null,"Queue","User","Pwd"};
		
		ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}

	@Test
	void testConstructor_emptyQueue_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Address","1005","","User","Pwd"};
		
		ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}

	@Test
	void testConstructor_nullQueue_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Address","1005",null,"User","Pwd"};
		
		ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_validArguments_shouldBeBuild() {
		String[] testArgs= new String[] {"Address","1005","Queue","User","Pwd"};
		
		ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
		
		assertFalse(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyUser_shouldBeBuild() {
		String[] testArgs= new String[] {"Address","1005","Queue","","Pwd"};
		
		ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
		
		assertFalse(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyPwd_shouldBeBuild() {
		String[] testArgs= new String[] {"Address","1005","Queue","User",""};
		
		ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
		
		assertFalse(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyUserAndEmptyPwd_shouldBeBuild() {
		String[] testArgs= new String[] {"Address","1005","Queue","",""};
		
		ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
		
		assertFalse(config.isEmpty());
	}
	
	@Test
	void testConstructor_portIsNotAnInt_shouldBeEmptyConfig() {
			String[] testArgs= new String[] {"Address","Some String instead of a number ","Queue","User","Pwd"};
			
			ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
			
			assertTrue(config.isEmpty());
	}

	@Test
	void testConstructor_portIsShortNumber_shouldBeBuild() {
			String[] testArgs= new String[] {"Address","105","Queue","User","Pwd"};
			
			ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
			
			assertFalse(config.isEmpty());
	}

	@Test
	void testConstructor_portIsLongNumber_shouldBeBuild() {
			String[] testArgs= new String[] {"Address","10155","Queue","User","Pwd"};
			
			ActiveMqConfiguration config = ActiveMqConfiguration.createActiveMqConfigFromArgs(testArgs);
			
			assertFalse(config.isEmpty());
	}
	
	@Test
	void testEmptyConfig_shouldBeEmpty() {
		ActiveMqConfiguration config = ActiveMqConfiguration.emptyConfig();
		
		assertTrue(config.isEmpty());
	}

    @Test
    void testFactory_fromProperties_AllValid_ShouldBeBuild(){
        Map<String,String> properties = new HashMap<>();
        properties.put("ActiveMqAddress","localhost");
        properties.put("ActiveMqPort","8182");
        properties.put("ActiveMqQueue","importantQueue");
        properties.put("ActiveMqUser","Michelle Obama");
        properties.put("ActiveMqPassword","1LoveBarrack");

        ActiveMqConfiguration config = ActiveMqConfiguration.createConfigFromProperties(properties);

        assertFalse(config.isEmpty());
    }

    @Test
    void testFactory_fromProperties_AllValid_NoUserNoPassword_ShouldBeBuild(){
        Map<String,String> properties = new HashMap<>();
        properties.put("ActiveMqAddress","localhost");
        properties.put("ActiveMqPort","8182");
        properties.put("ActiveMqQueue","importantQueue");

        ActiveMqConfiguration config = ActiveMqConfiguration.createConfigFromProperties(properties);

        assertFalse(config.isEmpty());
    }

    @Test
    void testFactory_fromProperties_AllValid_NoPassword_ShouldBeBuild(){
        Map<String,String> properties = new HashMap<>();
        properties.put("ActiveMqAddress","localhost");
        properties.put("ActiveMqPort","8182");
        properties.put("ActiveMqQueue","importantQueue");
        properties.put("ActiveMqUser","Michelle Obama");

        ActiveMqConfiguration config = ActiveMqConfiguration.createConfigFromProperties(properties);

        assertFalse(config.isEmpty());
    }

    @Test
    void testFactory_fromProperties_AllValid_NoPassword_PasswordShouldBeNull(){
        Map<String,String> properties = new HashMap<>();
        properties.put("ActiveMqAddress","localhost");
        properties.put("ActiveMqPort","8182");
        properties.put("ActiveMqQueue","importantQueue");
        properties.put("ActiveMqUser","Michelle Obama");

        ActiveMqConfiguration config = ActiveMqConfiguration.createConfigFromProperties(properties);

        assertNull(config.passwordToConnect);
    }

	@Test
    void testFactory_fromProperties_MissingAddress_ShouldBeEmptyConfig(){
        Map<String,String> properties = new HashMap<>();
        properties.put("ActiveMqPort","8182");
        properties.put("ActiveMqQueue","importantQueue");
        properties.put("ActiveMqUser","Michelle Obama");
        properties.put("ActiveMqPassword","1LoveBarrack");

        ActiveMqConfiguration config = ActiveMqConfiguration.createConfigFromProperties(properties);

        assertTrue(config.isEmpty());
    }


    @Test
    void testFactory_fromProperties_EmptyAddress_shouldBeEmptyConfig(){
        Map<String,String> properties = new HashMap<>();
        properties.put("ActiveMqAddress","");
        properties.put("ActiveMqPort","8182");
        properties.put("ActiveMqQueue","importantQueue");
        properties.put("ActiveMqUser","Michelle Obama");
        properties.put("ActiveMqPassword","1LoveBarrack");

        ActiveMqConfiguration config = ActiveMqConfiguration.createConfigFromProperties(properties);

        assertTrue(config.isEmpty());
    }
    @Test
    void testFactory_fromProperties_NoPort_shouldBeEmptyConfig(){
        Map<String,String> properties = new HashMap<>();
        properties.put("ActiveMqAddress","localhost");
        properties.put("ActiveMqQueue","importantQueue");
        properties.put("ActiveMqUser","Michelle Obama");
        properties.put("ActiveMqPassword","1LoveBarrack");

        ActiveMqConfiguration config = ActiveMqConfiguration.createConfigFromProperties(properties);

        assertTrue(config.isEmpty());
    }

    @Test
    void testFactory_fromProperties_EmptyPort_shouldBeEmptyConfig(){
        Map<String,String> properties = new HashMap<>();
        properties.put("ActiveMqAddress","localhost");
        properties.put("ActiveMqPort","");
        properties.put("ActiveMqQueue","importantQueue");
        properties.put("ActiveMqUser","Michelle Obama");
        properties.put("ActiveMqPassword","1LoveBarrack");

        ActiveMqConfiguration config = ActiveMqConfiguration.createConfigFromProperties(properties);

        assertTrue(config.isEmpty());
    }

    @Test
    void testFactory_fromProperties_NoQueue_shouldBeEmptyConfig(){
        Map<String,String> properties = new HashMap<>();
        properties.put("ActiveMqAddress","localhost");
        properties.put("ActiveMqPort","8182");
        properties.put("ActiveMqUser","Michelle Obama");
        properties.put("ActiveMqPassword","1LoveBarrack");

        ActiveMqConfiguration config = ActiveMqConfiguration.createConfigFromProperties(properties);

        assertTrue(config.isEmpty());
    }

    @Test
    void testFactory_fromProperties_EmptyQueue_shouldBeEmptyConfig(){
        Map<String,String> properties = new HashMap<>();
        properties.put("ActiveMqAddress","localhost");
        properties.put("ActiveMqPort","8182");
        properties.put("ActiveMqQueue","");
        properties.put("ActiveMqUser","Michelle Obama");
        properties.put("ActiveMqPassword","1LoveBarrack");

        ActiveMqConfiguration config = ActiveMqConfiguration.createConfigFromProperties(properties);

        assertTrue(config.isEmpty());
    }


}
