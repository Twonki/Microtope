package microtope.messages;

import java.util.Objects;

public class LoginMessage extends AmqBaseMessage implements PlayerRelatedMessage {
	
	private final int playerId;
	private final int teamId;

	public LoginMessage(int player,int team) {
		this.playerId = player;
		this.teamId = team;
	}
	
	public int getPlayerId() {
		return playerId;
	}
	
	public int getTeamId() {
		return teamId;
	}
	
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof LoginMessage)) {
			 return false;	
		}
		var otherParsed = (LoginMessage) o;
		
		return otherParsed.getPlayerId() == getPlayerId() 
				&& otherParsed.getTeamId() == getTeamId();
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(playerId, teamId, timestamp);
	}
}
