package microtope.messages;

import java.util.Objects;

public class LogoutMessage extends AmqBaseMessage implements PlayerRelatedMessage {
	
	private final int playerId;
	
	public LogoutMessage(int playerId) {
		this.playerId = playerId;
	}
	
	public int getPlayerId() {
		return playerId;
	}
	
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof LogoutMessage)) {
			return false;
		}
		var otherParsed = (LogoutMessage) o;
		
		return otherParsed.getPlayerId() == getPlayerId();
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(playerId, timestamp);
	}
}
