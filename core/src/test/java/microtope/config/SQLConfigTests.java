package microtope.config;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class SQLConfigTests {

	@Test
	void testConstructor_tooLowNumberOfArguments_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Adress","1005","Queue"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_tooHighNumberOfArguments_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Adress","1005","Queue","User","Pwd","One Argument too much!"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyAdress_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"","1005","Queue","User","Pwd","One Argument too much!"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_nullAdress_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {null,"1005","Queue","User","Pwd","One Argument too much!"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyPort_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Adress","","Queue","User","Pwd","One Argument too much!"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_nullPort_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Adress",null,"Queue","User","Pwd","One Argument too much!"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	@Test
	void testConstructor_emptyQueue_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Adress","1005","","User","Pwd","One Argument too much!"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}

	@Test
	void testConstructor_nullQueue_shouldBeEmptyConfig() {
		String[] testArgs= new String[] {"Adress","1005",null,"User","Pwd","One Argument too much!"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertTrue(config.isEmpty());
	}
	
	@Test
	void testConstructor_validArguments_shouldBeBuild() {
		String[] testArgs= new String[] {"Adress","1005","Queue","User","Pwd"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertFalse(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyUser_shouldBeBuild() {
		String[] testArgs= new String[] {"Adress","1005","Queue","","Pwd"};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertFalse(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyPwd_shouldBeBuild() {
		String[] testArgs= new String[] {"Adress","1005","Queue","User",""};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertFalse(config.isEmpty());
	}
	
	@Test
	void testConstructor_emptyUserAndEmptyPwd_shouldBeBuild() {
		String[] testArgs= new String[] {"Adress","1005","Queue","",""};
		
		SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
		
		assertFalse(config.isEmpty());
	}
	
	@Test
	void testConstructor_portIsNotAnInt_shouldBeEmptyConfig() {
			String[] testArgs= new String[] {"Adress","Some String instead of a number ","Queue","User","Pwd"};
			
			SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
			
			assertTrue(config.isEmpty());
	}

	@Test
	void testConstructor_portIsShortNumber_shouldBeBuild() {
			String[] testArgs= new String[] {"Adress","105","Queue","User","Pwd"};
			
			SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
			
			assertFalse(config.isEmpty());
	}
	@Test
	void testConstructor_portIsLongNumber_shouldBeBuild() {
			String[] testArgs= new String[] {"Adress","10155","Queue","User","Pwd"};
			
			SqlConfig config = SqlConfig.createSqlConfigFromArgs(testArgs);
			
			assertFalse(config.isEmpty());
	}
	
	@Test
	void testEmptyConfig_shouldBeEmpty() {
		SqlConfig config = SqlConfig.emptyConfig();
		
		assertTrue(config.isEmpty());
	}
}
