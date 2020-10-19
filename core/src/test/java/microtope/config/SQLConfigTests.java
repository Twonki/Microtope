package microtope.config;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

class SQLConfigTests {

	@Test
	void testConstructor_tooLowNumberOfArguments_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Address","1005","Queue"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_tooHighNumberOfArguments_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Address","1005","Queue","User","Pwd","One Argument too much!"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyAddress_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"","1005","Queue","User","Pwd"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_nullAddress_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {null,"1005","Queue","User","Pwd"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyPort_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Address","","Queue","User","Pwd"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_nullPort_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Address",null,"Queue","User","Pwd"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	@Test
	void testConstructor_emptyQueue_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Address","1005","","User","Pwd"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}

	@Test
	void testConstructor_nullQueue_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Address","1005",null,"User","Pwd"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_validArguments_shouldBeBuild() {
		String[] testArgs= new String[] {"Address","1005","Queue","User","Pwd"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertFalse(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyUser_shouldBeBuild() {
		String[] testArgs= new String[] {"Address","1005","Queue","","Pwd"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertFalse(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyPwd_shouldBeBuild() {
		String[] testArgs= new String[] {"Address","1005","Queue","User",""};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertFalse(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyUserAndEmptyPwd_shouldBeBuild() {
		String[] testArgs= new String[] {"Address","1005","Queue","",""};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertFalse(config.isEmpty());
	}
	
	@Test
	void testConstructor_portIsNotAnInt_shouldBeEmptyConfig() {
			String[] testArgs= new String[] {"Address","Some String instead of a number ","Queue","User","Pwd"};
			
			SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
			
			assertTrue(config.isEmpty());
	}

	@Test
	void testConstructor_portIsShortNumber_shouldBeBuild() {
			String[] testArgs= new String[] {"Address","105","Queue","User","Pwd"};
			
			SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
			
			assertFalse(config.isEmpty());
	}
	@Test
	void testConstructor_portIsLongNumber_shouldBeBuild() {
			String[] testArgs= new String[] {"Address","10155","Queue","User","Pwd"};
			
			SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
			
			assertFalse(config.isEmpty());
	}
	
	@Test
	void testEmptyConfig_shouldBeEmpty() {
		SqlConfig config = SqlConfig.emptyConfig();
		
		assertTrue(config.isEmpty());
	}

    @Test
    void testFactory_fromProperties_AllValid_ShouldBeBuild(){
        Map<String,String> properties = new HashMap<>();
        properties.put("SqlAddress","localhost");
        properties.put("SqlPort","8182");
        properties.put("SqlDatabase","someDatabase");
        properties.put("SqlUser","Michelle Obama");
        properties.put("SqlPassword","1LoveBarrack");

        SqlConfig config = SqlConfig.createConfigFromProperties(properties);

        assertFalse(config.isEmpty());
    }

    @Test
    void testFactory_fromProperties_AllValid_NoUserNoPassword_ShouldBeBuild(){
        Map<String,String> properties = new HashMap<>();
        properties.put("SqlAddress","localhost");

        properties.put("SqlPort","8182");
        properties.put("SqlDatabase","someDatabase");

        SqlConfig config = SqlConfig.createConfigFromProperties(properties);

        assertFalse(config.isEmpty());
    }

    @Test
    void testFactory_fromProperties_AllValid_NoPassword_ShouldBeBuild(){
        Map<String,String> properties = new HashMap<>();
        properties.put("SqlAddress","localhost");
        properties.put("SqlPort","8182");
        properties.put("SqlDatabase","someDatabase");
        properties.put("SqlUser","Michelle Obama");

        SqlConfig config = SqlConfig.createConfigFromProperties(properties);

        assertFalse(config.isEmpty());
    }

    @Test
    void testFactory_fromProperties_AllValid_NoPassword_PasswordShouldBeNull(){
        Map<String,String> properties = new HashMap<>();
        properties.put("SqlAddress","localhost");
        properties.put("SqlPort","8182");
        properties.put("SqlDatabase","someDatabase");
        properties.put("SqlUser","Michelle Obama");

        SqlConfig config = SqlConfig.createConfigFromProperties(properties);

        assertNull(config.passwordToConnect);
    }

    @Test
    void testFactory_fromProperties_MissingAddress_ShouldBeEmptyConfig(){
        Map<String,String> properties = new HashMap<>();
        properties.put("SqlPort","8182");
        properties.put("SqlDatabase","someDatabase");
        properties.put("SqlUser","Michelle Obama");
        properties.put("SqlPassword","1LoveBarrack");

        SqlConfig config = SqlConfig.createConfigFromProperties(properties);

        assertTrue(config.isEmpty());
    }


    @Test
    void testFactory_fromProperties_EmptyAddress_shouldBeEmptyConfig(){
        Map<String,String> properties = new HashMap<>();
        properties.put("SqlAddress","");
        properties.put("SqlPort","8182");
        properties.put("SqlDatabase","someDatabase");
        properties.put("SqlUser","Michelle Obama");
        properties.put("SqlPassword","1LoveBarrack");

        SqlConfig config = SqlConfig.createConfigFromProperties(properties);

        assertTrue(config.isEmpty());
    }
    @Test
    void testFactory_fromProperties_NoPort_shouldBeEmptyConfig(){
        Map<String,String> properties = new HashMap<>();
        properties.put("SqlAddress","localhost");
        properties.put("SqlDatabase","someDatabase");
        properties.put("SqlUser","Michelle Obama");
        properties.put("SqlPassword","1LoveBarrack");

        SqlConfig config = SqlConfig.createConfigFromProperties(properties);

        assertTrue(config.isEmpty());
    }

    @Test
    void testFactory_fromProperties_EmptyPort_shouldBeEmptyConfig(){
        Map<String,String> properties = new HashMap<>();
        properties.put("SqlAddress","localhost");
        properties.put("SqlPort","");
        properties.put("SqlDatabase","someDatabase");
        properties.put("SqlUser","Michelle Obama");
        properties.put("SqlPassword","1LoveBarrack");

        SqlConfig config = SqlConfig.createConfigFromProperties(properties);

        assertTrue(config.isEmpty());
    }

    @Test
    void testFactory_fromProperties_NoQueue_shouldBeEmptyConfig(){
        Map<String,String> properties = new HashMap<>();
        properties.put("SqlAddress","localhost");
        properties.put("SqlPort","8182");
        properties.put("SqlUser","Michelle Obama");
        properties.put("SqlPassword","1LoveBarrack");

        SqlConfig config = SqlConfig.createConfigFromProperties(properties);

        assertTrue(config.isEmpty());
    }

    @Test
    void testFactory_fromProperties_EmptyQueue_shouldBeEmptyConfig(){
        Map<String,String> properties = new HashMap<>();
        properties.put("SqlAddress","localhost");
        properties.put("SqlPort","8182");
        properties.put("SqlDatabase","");
        properties.put("SqlUser","Michelle Obama");
        properties.put("SqlPassword","1LoveBarrack");

        SqlConfig config = SqlConfig.createConfigFromProperties(properties);

        assertTrue(config.isEmpty());
    }

}
