package microtope.data;

public abstract class MessageGenerator {
	
	public static String createLoginMessage(int playerId,int teamId) {
		String loginMessageFormat = "E: Player %d logged in for team %d";
		return String.format(loginMessageFormat, playerId, teamId + 1);
	}
	
	public static String createLogoutMessage(int playerId) {	
		String logoutMessageFormat = "E: Player %d logged out";
		return String.format(logoutMessageFormat, playerId);
	}
	
	public static String createCoinMessage(int playerId, int teamID, int amount) {
		String coinMessageFormat = "E: Player %d collected %d coins for Team %d";
		return String.format(coinMessageFormat, playerId, amount, teamID + 1);
	}
	
	public static String createStepMessage(int playerId, int steps) {
		String stepMessageFormat = "M: Player %d moved %d steps";
		return String.format(stepMessageFormat, playerId, steps);
	}
}
