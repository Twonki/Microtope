package microtope.data;

import java.util.Random;

/**
 * Class that holds a number of generators of objects found in the package data.
 *
 * Is used for both the actual usage with dummy data, as well as for some Integration Tests.
 * Hence, it is put into the core-package so its usable in multiple places.
 *
 * Instead of just using random ints, this class (hopefully) makes it easier readable and has some
 * Sanity value ranges build in.
 */
public abstract class DataGenerator {
	
	private static final int BASE_STEPS = 10;
	private static final int MAX_STEPS = 50;
	
	private static final int BASE_COINS = 1;
	private static final int MAX_COINS = 3;
	
	private static final Random rnd = new Random();

    /**
     * Data Generator for Teams.
     * Can only return actually existant teams and is updated (automatically) if Team.java is updated.
     *
     * @return a random team of the teams defined in Team.java
     */
	public static Team getRandomTeam() {
		var teams = Team.values();
		var rndIndex = rnd.nextInt(teams.length);
		
		return teams[rndIndex];
	}

    /**
     * Data Generator for Steps
     * @return a number between 10 and 60
     */
	public static int getRandomSteps() {
		var noise = rnd.nextInt(MAX_STEPS - BASE_STEPS);
		return BASE_STEPS + noise;
	}

    /**
     * Data Generator for Coins
     * @return a number between 1 and 4
     */
	public static int getRandomCoins() {
		var noise = rnd.nextInt(MAX_COINS - BASE_COINS);
		return BASE_COINS + noise;
	}

    /**
     * Data Generator for a Player Number
     * @return a number between 0 and 99999
     */
	public static int getRandomPlayerNumber() {
		return rnd.nextInt(99999);
	}
}
