package microtope.data;

import java.util.Random;

public abstract class DataGenerator {
	
	
	private static final int BASESTEPS = 10;
	private static final int MAXSTEPS = 50;
	
	private static final int BASECOINS = 1;
	private static final int MAXCOINS = 3;
	
	private static final Random rnd = new Random();

	public static Team getRandomTeam() {
		var teams = Team.values();
		var rndIndex = rnd.nextInt(teams.length);
		
		return teams[rndIndex];
	}
	
	public static int getRandomSteps() {
		var noise = rnd.nextInt(MAXSTEPS - BASESTEPS);
		return BASESTEPS + noise;
	}
	
	public static int getRandomCoins() {
		var noise = rnd.nextInt(MAXCOINS - BASECOINS);
		return BASECOINS + noise;
	}
	
	public static int getRandomPlayerNumber() {
		return rnd.nextInt(99999);
	}
}
