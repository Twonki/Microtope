package microtope.messages;

import java.util.Date;

/**
 * Super-Class for AMQ Messages which contains logic primarily for the TimeStaps
 * (Which is a shared attribute of AMQ-Messages).
 * The timestamp is a protected variable and hence accessible for every inheriting object.
 */
public abstract class AmqBaseMessage implements AmqMessage {

	protected Date timestamp = new Date(0);

	public void setTimeStamp(Date date) {
		this.timestamp = (Date) date.clone();
	}

	public Date getTimeStamp() {
		return (Date) timestamp.clone();
	}
	
}