package microtope.data;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import microtope.data.ValueChecker;

class ValueCheckerTests {
	
	@Test
	public void testURLChecker_kubernetesDNS_shouldBeTrue() {
		//This is extra important for Kubernetes Service Discovery will look like this
		String urlToCheck="my-svc.my-namespace.svc.cluster-domain.example";
		
		boolean result = ValueChecker.goodUrl(urlToCheck);
		
		assertTrue(result);
	}
	
	@Test
	public void testURLChecker_kubernetesDNS2_shouldBeTrue() {
		//This is extra important for Kubernetes Service Discovery will look like this
		String urlToCheck="auto-generated-name.my-svc.my-namespace.svc.cluster-domain.example";
		
		boolean result = ValueChecker.goodUrl(urlToCheck);
		
		assertTrue(result);
	}
	
	@Test
	public void testURLChecker_noWWW_shouldBeTrue() {
		String urlToCheck="google.com";
		
		boolean result = ValueChecker.goodUrl(urlToCheck);
		
		assertTrue(result);
	}
	
	@Test void testURLChecker_singleName_shouldBeFalse() {
		String urlToCheck="ebay";
		
		boolean result = ValueChecker.goodUrl(urlToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testURLChecker_validIP4_shouldBeTrue() {
		String urlToCheck="127.0.0.1";
		
		boolean result = ValueChecker.goodUrl(urlToCheck);
		
		assertTrue(result);
	}

	@Test
	public void testURLChecker_someGarbage_shouldBeFalse() {
		String urlToCheck="Some Garbage @@";
		
		boolean result = ValueChecker.goodUrl(urlToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testURLChecker_validDomainName_shouldBeTrue() {
		String urlToCheck="www.google.com";
		
		boolean result = ValueChecker.goodUrl(urlToCheck);
		
		assertTrue(result);
	}

	@Test
	public void testURLChecker_validDomainNameWithPath_shouldBeFalse() {
		String urlToCheck="www.hosebund.de/hosen";
		
		boolean result = ValueChecker.goodUrl(urlToCheck);
		
		assertFalse(result);
	}

	@Test
	public void testURLChecker_validURLWithProtocoll_shouldBeFalse() {
		String urlToCheck="https://www.google.de";
		
		boolean result = ValueChecker.goodUrl(urlToCheck);
		
		assertFalse(result);
	}

	@Test
	public void testURLChecker_validURLWithPort_shouldBeFalse() {
		String urlToCheck="hosenbund.de:61616";
		
		boolean result = ValueChecker.goodUrl(urlToCheck);
		
		assertFalse(result);
	}

	@Test
	public void testURLChecker_nullURL_shouldBeFalse() {
		String urlToCheck=null;
		
		boolean result = ValueChecker.goodUrl(urlToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testURLChecker_emptyURL_shouldBeFalse() {
		String urlToCheck="";
		
		boolean result = ValueChecker.goodUrl(urlToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testIP4ValueChecker_validIP4_shouldBeTrue() {
		String ipToCheck = "172.172.011.211";
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertTrue(result);
	}
	
	@Test
	public void testIP4ValueChecker_validShortIP4_shouldBeTrue() {
		String ipToCheck = "11.1.0.2";
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertTrue(result);
	}
	
	@Test
	public void testIP4ValueChecker_localhost_shouldBeTrue() {
		String ipToCheck = "127.0.0.1";
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertTrue(result);
	}
	
	@Test
	public void testIP4ValueChecker_someOtherString_shouldBeFalse() {
		String ipToCheck = "Bad String";
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testIP4ValueChecker_tooShortIP_shouldBeFalse() {
		String ipToCheck = "172.172.211";
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertFalse(result);
	}

	@Test
	public void testIP4ValueChecker_IPEndsWithDot_shouldBeFalse() {
		String ipToCheck = "172.172.211.";
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertFalse(result);
	}

	@Test
	public void testIP4ValueChecker_IPEndsWithDot2_shouldBeFalse() {
		String ipToCheck = "172.172.211.172.";
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testIP4ValueChecker_IPstartsWithDot_shouldBeFalse() {
		String ipToCheck = ".172.172.211";
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testIP4ValueChecker_IPHasTooHighNumber_shouldBeFalse() {
		String ipToCheck = "256.172.172.211";
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testIP4ValueChecker_emptyIP_shouldBeFalse() {
		String ipToCheck = "";
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertFalse(result);
	}
	

	@Test
	public void testIP4ValueChecker_nullIP_shouldBeFalse() {
		String ipToCheck = null;
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertFalse(result);
	}
	
	
	@Test
	public void testIP4ValueChecker_otherDelimiter_shouldBeFalse() {
		String ipToCheck = "172:172:011:211";
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testIP4ValueChecker_tooLongIP_shouldBeFalse() {
		String ipToCheck = "172.172.1.5.221";
		
		boolean result = ValueChecker.goodIPv4(ipToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testPortChecker_defaultAMQPort_shouldBeTrue() {
		String portToCheck = "61616";
		
		boolean result = ValueChecker.goodPort(portToCheck);
		
		assertTrue(result);
	}
	
	@Test
	public void testPortChecker_shortValidPort_shouldBeTrue() {
		String portToCheck = "85";
		
		boolean result = ValueChecker.goodPort(portToCheck);
		
		assertTrue(result);
	}
	
	@Test
	public void testPortChecker_someOtherString_shouldBeFalse() {
		String portToCheck = "Hello";
		
		boolean result = ValueChecker.goodPort(portToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testPortChecker_zeroPort_shouldBeFalse() {
		String portToCheck = "0";
		
		boolean result = ValueChecker.goodPort(portToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testPortChecker_negativePort_shouldBeFalse() {
		String portToCheck = "-85";
		
		boolean result = ValueChecker.goodPort(portToCheck);
		
		assertFalse(result);
	}
	
	@Test
	public void testPortChecker_nullPort_shouldBeFalse() {
		String portToCheck = null;
		
		boolean result = ValueChecker.goodPort(portToCheck);
		
		assertFalse(result);
	}

	@Test
	public void testPortChecker_emptyPort_shouldBeFalse() {
		String portToCheck = "";
		
		boolean result = ValueChecker.goodPort(portToCheck);
		
		assertFalse(result);
	}
}
