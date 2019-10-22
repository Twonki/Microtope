package microtope.messages;

import java.util.Objects;

public class CoinMessage extends AmqBaseMessage implements PlayerRelatedMessage {
	
	private final int playerId;
	private final int coins;
	
	public CoinMessage(int playerId, int coins) {
		this.playerId = playerId;
		this.coins = coins;
	}

	public int getPlayerId() {
		return playerId;
	}

	public int getCoins() {
		return coins;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof CoinMessage)) {
			return false;
		}
		var otherParsed = (CoinMessage) o;
		
		return otherParsed.getPlayerId() == getPlayerId() 
				&& otherParsed.getCoins() == getCoins();
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(playerId, coins, timestamp);
	}
}
