package microtope.messages;

import java.util.Date;

public interface AmqMessage {
	
	void setTimeStamp(Date date);
	
	Date getTimeStamp();

}
