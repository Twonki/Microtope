package microtope.messages;

import java.util.Objects;

public class StepMessage extends AmqBaseMessage  implements PlayerRelatedMessage {
	private final int steps;
	private final int playerId;
	
	public StepMessage(int playerId, int steps) {
		this.playerId = playerId;
		this.steps = steps;
	}

	public int getSteps() {
		return steps;
	}

	public int getPlayerId() {
		return playerId;
	}
	
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof StepMessage)) {
			return false;
		}
		var otherParsed = (StepMessage) o;
		
		return otherParsed.getPlayerId() == getPlayerId()
				&& otherParsed.getSteps() == getSteps();
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(playerId, steps, timestamp);
	}
}
