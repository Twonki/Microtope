Use 'microtope';

GRANT ALL PRIVILEGES ON microtope.* TO 'admin'@'%';
/*
Some of these Privileges are auto generated. 
These need to be generated after every db schema change (views and tables) with the script given above the entries. 
Also they might need to be adjusted, right now the "audits" table is forbidden for most entries. I will give a short summary above every auto generated rights
*/

/*
The API can read every View and Table which is in table microtope, except for audits.
SELECT CONCAT("GRANT SELECT ON  microtope.", table_name, " TO 'api'@'%';")
FROM information_schema.TABLES
WHERE table_schema = "microtope" AND table_name <> "audits";
*/
GRANT SELECT ON  microtope.coins TO 'api'@'%';                       
GRANT SELECT ON  microtope.players TO 'api'@'%';                     
GRANT SELECT ON  microtope.steps_by_team TO 'api'@'%';               
GRANT SELECT ON  microtope.steps_by_user TO 'api'@'%';               
GRANT SELECT ON  microtope.teams TO 'api'@'%';           
GRANT SELECT ON  microtope.player_summary TO 'api'@'%';              
GRANT SELECT ON  microtope.coins_by_team TO 'api'@'%';               
GRANT SELECT ON  microtope.steps TO 'api'@'%';                       
GRANT SELECT ON  microtope.coins_by_user TO 'api'@'%';
GRANT SELECT ON  microtope.team_summary TO 'api'@'%';
/*
The worker can CRUD on every Table but not on views. He cannot read or alter audits table. 
SELECT CONCAT("GRANT SELECT,INSERT,DELETE,UPDATE ON  microtope.", table_name, " TO 'worker'@'%';")
FROM information_schema.TABLES
WHERE table_type="BASE TABLE" AND table_schema = "microtope" AND table_name <> "audits";
*/
GRANT SELECT,INSERT,DELETE,UPDATE ON  microtope.coins TO 'worker'@'%';                      
GRANT SELECT,INSERT,DELETE,UPDATE ON  microtope.players TO 'worker'@'%';                    
GRANT SELECT,INSERT,DELETE,UPDATE ON  microtope.teams TO 'worker'@'%';                 
GRANT SELECT,INSERT,DELETE,UPDATE ON  microtope.steps TO 'worker'@'%';  


-- Health Checks
-- Every Component can healthcheck to verify its connection and login
GRANT SELECT ON microtope.health TO 'worker'@'%';
GRANT SELECT ON microtope.health TO 'api'@'%';
GRANT SELECT ON microtope.health TO 'auditor'@'%';

-- The API can change player names, but nothing else about the player
GRANT INSERT,UPDATE (player_name) ON microtope.players TO 'api'@'%';
GRANT INSERT,UPDATE (team_name) ON microtope.teams TO 'api'@'%';

-- "HIGH SECURITY" LOGIN/LOGOUT TABLE RELATED

-- The worker can only insert new login/logout entries, but is not able to read or update it
GRANT INSERT(player_id,recorded,action) ON microtope.audits TO 'worker'@'%';

-- Auditor can read everything and update/write the controlled field to true
GRANT INSERT,UPDATE (controlled) ON microtope.audits TO 'auditor'@'%';
GRANT DELETE ON microtope.audits TO 'auditor'@'%';
GRANT SELECT ON microtope.audits to 'auditor'@'%';
-- The Auditor cannot Delete Entries

FLUSH PRIVILEGES;

