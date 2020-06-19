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
RETURN
  (
    SELECT audits.recorded 
    FROM audits
    WHERE player = audits.player_id
    AND audits.controlled
    AND audits.action = 'logout'
    AND audits.recorded >= login_time 
    ORDER BY TIMEDIFF(audits.recorded,login_time) ASC
    LIMIT 1
  );