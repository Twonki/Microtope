package microtope.data;

/**
 * This class provides a number of static generators for Messages in the correct format.
 * It is intended to unify the messages and to reduce error in typos and similar issues.
 *
 * Be careful with changing it and always look into the according AmqMessageParser.java to stay updated.
 * For every type of message, there is a corresponding parsing-regex there.
 *
 * The prefixes E: stands for E-vent
 * and M stands for M-ovement
 * Movement is not seen as an actual Game-Event and hence separated with a different prefix.
 */
public abstract class MessageGenerator {

    /**
     * Creates a String for a Player-Login-Event.
     * Prefix is E for Event
     * The Team-ID is increased by one, as the enumeration of teams starts at 0,
     * but there is no "Team 0" in human language (Humans actually count from 1).
     * @param playerId the PlayerID who logged in - should be a positive number
     * @param teamId the teamID to which the player belongs. It is increased by 1, so there is never a team 0.
     * @return a formatted, unified String for the Message to be put in AMQ
     */
	public static String createLoginMessage(int playerId,int teamId) {
		String loginMessageFormat = "E: Player %d logged in for team %d";
		return String.format(loginMessageFormat, playerId, teamId + 1);
	}

    /**
     * Creates a String for a Player-Logout-Event.
     * Prefix is E for Event.
     * @param playerId The Player who logged out.
     * @return a formatted, unified String for the Message to be put in AMQ
     */
	public static String createLogoutMessage(int playerId) {	
		String logoutMessageFormat = "E: Player %d logged out";
		return String.format(logoutMessageFormat, playerId);
	}

    /**
     * Creates a String for a coin-message, which indicates that the player found a coin (for his team).
     * These events should occur sparse otherwise teams have to many coins.
     *
     * Prefix is E for Event.
     *
     * @param playerId The Player who found a coin
     * @param teamID The team to which the player belongs. It is increased by 1, so there is never a team 0.
     * @param amount The number of coins the player found.
     * @return a formatted, unified String for the Message to be put in AMQ
     */
	public static String createCoinMessage(int playerId, int teamID, int amount) {
		String coinMessageFormat = "E: Player %d collected %d coins for Team %d";
		return String.format(coinMessageFormat, playerId, amount, teamID + 1);
	}

    /**
     * Creates a String for a step-message, which is intended to be a generic event how players move and
     * contribute towards the teams steps.
     * Step Messages are intended to be send at multiple times,
     * so a player might send 4 Messages per minute with 10 steps each.
     * Most of the Logic for steps is aggregated in the database or the frontend.
     *
     * Prefix is M for Movement.
     *
     * @param playerId the player who moved
     * @param steps the number of steps the player walked since last step-message
     * @return a formatted, unified String for the Message to be put in AMQ
     */
	public static String createStepMessage(int playerId, int steps) {
		String stepMessageFormat = "M: Player %d moved %d steps";
		return String.format(stepMessageFormat, playerId, steps);
	}
}
