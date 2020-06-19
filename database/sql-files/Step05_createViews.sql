Use `microtope`;
-- The view for valid sessions is done with the validity-functions

CREATE OR REPLACE VIEW microtope.steps 
AS
SELECT player_id, steps, recorded
FROM unchecked_steps
WHERE is_in_valid_session(player_id,recorded);

CREATE OR REPLACE VIEW microtope.coins 
AS
SELECT player_id,value,recorded
FROM unchecked_coins
WHERE is_in_valid_session(player_id,recorded);

CREATE VIEW microtope.steps_by_user 
AS
SELECT ps.player_id,SUM(steps) AS steps 
FROM players AS ps
JOIN steps AS s
ON ps.player_id = s.player_id
GROUP BY ps.player_id 
ORDER BY steps DESC;

CREATE VIEW microtope.coins_by_user
AS 
SELECT player_id, sum(value) AS coins
FROM coins 
GROUP BY player_id
ORDER BY coins DESC;

CREATE VIEW microtope.coins_by_team 
AS
SELECT t.team_id, t.team_name, SUM(value) AS coins
FROM teams AS t
JOIN players AS ps 
JOIN coins AS c
ON t.team_id = ps.team_id
AND ps.player_id = c.player_id
GROUP BY t.team_id 
ORDER BY coins DESC;

CREATE VIEW microtope.steps_by_team
AS 
SELECT t.team_id,t.team_name,sum(steps) AS steps
FROM teams as t 
JOIN steps as s
JOIN players as ps 
ON ps.team_id = t.team_id 
AND s.player_id = ps.player_id
GROUP BY t.team_id 
ORDER BY steps DESC;

CREATE VIEW microtope.player_summary
AS
SELECT ps.player_id, ps.player_name, s.steps, c.coins 
FROM players as ps, coins_by_user as c, steps_by_user as s
WHERE ps.player_id = c.player_id 
AND ps.player_id=s.player_id;

CREATE VIEW microtope.team_summary
AS 
SELECT t.team_id as team_id, t.team_name as team_name, sbt.steps as steps, cbt.coins as coins
FROM teams as t 
JOIN steps_by_team as sbt
JOIN coins_by_team as cbt 
WHERE t.team_id=sbt.team_id
AND t.team_id =cbt.team_id