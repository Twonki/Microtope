package microtope.data;

import static org.junit.jupiter.api.Assertions.*;

import java.util.LinkedList;

import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

import microtope.data.DataGenerator;
import microtope.data.Team;

class DataGeneratorTests {
	
	
	@RepeatedTest(5)
	void testGetRandomTeam_shouldBeInTheGivenTeams() {
		Team t = DataGenerator.getRandomTeam();
		
		var teams = (Team.values());
		
		boolean isSeen=false;
		for(Team i : teams) {
			isSeen = isSeen || t == i;
		}
		
		assertTrue(isSeen);
	}
	
	/*
	 * This test may fail, but is very very unlikely too (statisticly speaking)
	 */
	@Test
	void testGetRandomTeam_createManyTeams_shouldContainAllTeams() {
		// Make 10000 Teams
		var candidates = new LinkedList<Team>();
		for(int i = 0; i<10000 ; i++)
			candidates.add(DataGenerator.getRandomTeam());
		
		boolean correct = true;
		// For every entry in teams check whether we have atleast one candidate
		for(Team t : Team.values()) {
			correct = correct && candidates.contains(t);
		}
		
		assertTrue(correct);
	}
	
	@RepeatedTest(50)
	void testGetRandomSteps_shouldNotBeNull() {
		int result = DataGenerator.getRandomSteps();
		
		assertNotEquals(0,result);
	}
	
	@RepeatedTest(50)
	void testGetRandomSteps_shouldbeBetween10And50() {
		int result = DataGenerator.getRandomSteps();
		
		boolean isInRange = result>=10 && result<=50;
		
		assertTrue(isInRange);
	}
	
	@RepeatedTest(50)
	void testGetRandomCoins_shouldNotBeNull() {
		int result = DataGenerator.getRandomCoins();
		
		assertNotEquals(0,result);
	}

	@RepeatedTest(50)
	void testGetRandomCoins_shouldBetween1and3() {
		int result = DataGenerator.getRandomCoins();
		
		boolean isInRange = result>=1 && result<=3;
		
		assertTrue(isInRange);
	}

	@RepeatedTest(150)
    void testGetRandomPlayerNumber_shouldBePositiveAndLowerThan10000(){
        int result = DataGenerator.getRandomPlayerNumber();

        boolean isInRange = result>=0 && result<=100000;

        assertTrue(isInRange);
    }

}
