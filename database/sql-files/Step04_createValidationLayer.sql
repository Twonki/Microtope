USE `microtope`;
/*
closest_logout finds the nearest following logout for a given time and a given player.
It returns the logout time.
It can be invoked for times where there is no login in the table, so it works independently from logins. 
Wherever this function is used, it should be used with some "sense" that one only looks for nearest logouts for login-times (not any times anywhere).
 
can be executed as such:
select closest_logout(100001,'2019-01-01 15:03:00');

Make sure that the user has the right rights. 
GRANT EXECUTE ON FUNCTION closest_logout TO 'user'@'localhost';

A script to run and see results for debugging purposes: 

SELECT *
FROM audits
WHERE 100001 = audits.player_id
AND audits.controlled
AND audits.action = 'logout'
AND audits.recorded >= '2019-01-01 15:03:00'
ORDER BY TIMEDIFF(audits.recorded,'2019-01-01 15:03:00') ASC;
*/
CREATE OR REPLACE FUNCTION 
closest_logout (player INT, login_time DATETIME) 
RETURNS DATETIME DETERMINISTIC
RETURN (
    SELECT audits.recorded 
    FROM audits
    WHERE player = audits.player_id
    AND audits.controlled
    AND audits.action = 'logout'
    AND audits.recorded >= login_time 
    ORDER BY TIMEDIFF(audits.recorded,login_time) ASC
    LIMIT 1
);

CREATE OR REPLACE VIEW microtope.valid_sessions 
AS
SELECT player_id, recorded as login_time, closest_logout(player_id,recorded) as logout_time
FROM audits
WHERE controlled
AND action = 'login'
AND closest_logout(player_id,recorded) IS NOT NULL;

/*
is_in_valid_session checks whether a certain timestamp for given player is in a valid session.
It returns a 0 or 1 (boolean).

It is used to build the proper views required for filtering out un-checked data.
The function first filters the valid sessions for all regarding the player, 
and then XOR's the date for all valid session-boundaries.
The XOR has been chosen on purpose, as a timestamp for a player should not be in two sessions.

can be executed as such:
select is_in_valid_session(100001,'2019-01-01 15:03:30'); -- -> True
select is_in_valid_session(100001,'2019-01-01 15:01:30'); -- -> False

A query which returns a result is the following (for debugging)
SELECT BIT_OR('2019-01-01 15:03:30' BETWEEN login_time AND logout_time) FROM valid_sessions WHERE player_id = 100001;
*/
CREATE OR REPLACE FUNCTION 
is_in_valid_session(player INT,time_to_check DATETIME)
RETURNS BOOLEAN DETERMINISTIC
RETURN (
  SELECT BIT_XOR(time_to_check BETWEEN login_time AND logout_time) 
  FROM valid_sessions 
  WHERE player_id = player
);