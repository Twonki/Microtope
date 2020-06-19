USE `microtope`;

CREATE TABLE IF NOT EXISTS teams (
    team_id INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    team_name VARCHAR(20) NOT NULL,
    CONSTRAINT constraint_unique_teamnames UNIQUE (team_name)
)WITH SYSTEM VERSIONING, engine=InnoDB default charset utf8;

CREATE TABLE IF NOT EXISTS players (
  player_id INT UNSIGNED NOT NULL PRIMARY KEY ,
  player_name VARCHAR(30),
  team_id INT UNSIGNED NOT NULL,
  CONSTRAINT `fk_players_teams`
		FOREIGN KEY (team_id) REFERENCES teams (team_id)
		ON DELETE CASCADE
		ON UPDATE RESTRICT
)WITH SYSTEM VERSIONING, engine=InnoDB default charset utf8;

CREATE TABLE IF NOT EXISTS unchecked_steps (
  `player_id` INT UNSIGNED NOT NULL,
  `steps` INT UNSIGNED  NOT NULL,
  `recorded` DATETIME NOT NULL,
  CONSTRAINT `fk_steps_players`
    FOREIGN KEY (player_id) REFERENCES players (player_id)
    ON DELETE CASCADE
)WITH SYSTEM VERSIONING, engine=InnoDB default charset utf8;

CREATE TABLE IF NOT EXISTS unchecked_coins (
  `player_id` INT UNSIGNED NOT NULL,
  `value` INT UNSIGNED  NOT NULL,
  `recorded` DATETIME NOT NULL,
  CONSTRAINT `fk_coins_players`
    FOREIGN KEY (player_id) REFERENCES players (player_id)
    ON DELETE CASCADE
)WITH SYSTEM VERSIONING, engine=InnoDB default charset utf8;

CREATE TABLE IF NOT EXISTS audits (
  `audit_id` INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `player_id` INT UNSIGNED NOT NULL,
  `action` ENUM('login','logout') NOT NULL,
  `recorded` DATETIME NOT NULL,
  `controlled` BOOLEAN NOT NULL DEFAULT 0,
  CONSTRAINT `fk_auditing_players`
    FOREIGN KEY (player_id) REFERENCES players (player_id)
    ON DELETE CASCADE
)WITH SYSTEM VERSIONING, engine=InnoDB default charset utf8;
