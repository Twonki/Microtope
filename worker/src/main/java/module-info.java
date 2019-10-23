module worker {
	requires transitive org.apache.logging.log4j;
	//requires transitive org.apache.activemq;
	requires java.sql;

	requires activemq.all;
	
	exports microtope.program;
}