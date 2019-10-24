package microtope.messages;

import java.util.Date;

public class AmqBaseMessage implements AmqMessage {

	protected Date timestamp = new Date(0);

	public void setTimeStamp(Date date) {
		this.timestamp = (Date) date.clone();
	}

	public Date getTimeStamp() {
		return (Date) timestamp.clone();
	}
	
}