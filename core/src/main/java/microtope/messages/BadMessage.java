package microtope.messages;

import java.util.Date;

public class BadMessage implements AmqMessage {
	
	@Override
	public boolean equals(Object o) {
		return o instanceof BadMessage;
	}

	public void setTimeStamp(Date date) {
		
	}

	public Date getTimeStamp() {
		return null;
	}
	
	@Override
	public int hashCode() {
		return 42;
	}
	
}
