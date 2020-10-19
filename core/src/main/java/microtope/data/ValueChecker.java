package microtope.data;


public abstract class ValueChecker {

	public static boolean goodUrl(String urlToCheck) {
		if (goodIPv4(urlToCheck)) {
			return true;
		}
        return isValidDomain(urlToCheck);
    }
	
	public static boolean isValidDomain(String addStr) {
		boolean ret = true;
		
		if ("".equals(addStr) || addStr == null) {
			ret = false;
		} else if (addStr.startsWith("-") || addStr.endsWith("-")) {
			ret = false;
		} else if (addStr.indexOf(".") == -1) {
			ret = false;
		} else {		
			// Split domain into String array.
			var domainEle = addStr.split("\\.");
			int size = domainEle.length;
			// Loop in the domain String array.
			for (int i = 0;i < size;i++) {
				// If one domain part string is empty, then return false.
				String domainEleStr = domainEle[i];
				if ("".equals(domainEleStr.trim())) {
					return false;
				}
			}
			
			// Get domain char array.
			char[] domainChar = addStr.toCharArray();			
			size = domainChar.length;
			// Loop in the char array.
			for (int i = 0;i < size;i++) {
				// Get each char in the array.
				char eleChar = domainChar[i];
				String charStr = String.valueOf(eleChar);
				
				// If char value is not a valid domain character then return false.
				if (!".".equals(charStr) && !"-".equals(charStr) && !charStr.matches("[a-zA-Z]") && !charStr.matches("[0-9]")) {
					ret = false;
					break;
				}				
			}
		}		
		return ret;
	}
	
	
	public static boolean goodIPv4(String ipToCheck) {
		try {
	        if (ipToCheck == null || ipToCheck.isEmpty()) {
	            return false;        	
	        }
	
	        String[] parts = ipToCheck.split("\\.");
	        
	        if (parts.length != 4) {
	            return false;        	
	        }
	        for (String s : parts) {
	            int i = Integer.parseInt(s);
	            if (i < 0 || i > 255) {
	                return false;
	            }
	        }
            return !ipToCheck.endsWith(".");
        } catch (NumberFormatException nfe) {
	        return false;
	    }
	}
	
	public static boolean goodPort(String portToCheck) {
		if (portToCheck == null || portToCheck.isEmpty()) {
			return false;	
		} else {
			try {
				int p = Integer.parseInt(portToCheck);
				return p > 0;
			} catch (Exception e) {
				return false;
			}
		}
	}
	
}
