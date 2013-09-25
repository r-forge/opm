

--------------------------------------------------------------------------------
--
-- Code for creating the tables that hold PM data. Tested with PostgreSQL (9.1)
-- and SQLite (3.7.9).
--
--------------------------------------------------------------------------------


-- DROP TABLE plates;
CREATE TABLE plates (
  id integer PRIMARY KEY,
  plate_type varchar (10) NOT NULL,
  -- one might need other datatype in other RDBMS, should cover date AND time:
  setup_time timestamp NOT NULL,
  -- string length does not vary, but this type is efficient in PostgreSQL:
  position varchar (4) NOT NULL,
  -- necessary if several instruments are involved:
  machine_id integer NOT NULL,
  -- this holds all originally read CSV data in JSON format for restoring them:
  csv_data text NOT NULL,
  -- per instrument, setup time and position identify each plate:
  UNIQUE (setup_time, position, machine_id)
);


--------------------------------------------------------------------------------


-- DROP TABLE wells;
CREATE TABLE wells (
  id integer PRIMARY KEY,
  plate_id integer NOT NULL REFERENCES plates,
  -- string length does not vary, but this type is efficient in PostgreSQL:
  coordinate varchar (3) NOT NULL,
  UNIQUE (plate_id, coordinate)
);


--------------------------------------------------------------------------------


-- DROP TABLE measurements;
CREATE TABLE measurements (
  id integer PRIMARY KEY,
  well_id integer NOT NULL REFERENCES wells,
  time real NOT NULL,
  value real NOT NULL,
  UNIQUE (well_id, time)
);


--------------------------------------------------------------------------------


-- DROP TABLE aggr_settings;
CREATE TABLE aggr_settings (
  id integer PRIMARY KEY,
  plate_id integer NOT NULL REFERENCES plates,
  software varchar (25) NOT NULL,
  version varchar (25) NOT NULL,
  method varchar (25) NOT NULL,
  -- "options" is intended for a JSON or YAML dump of the option list
  options text NOT NULL
  -- a UNIQUE constraint would involve quite a few comparisons
);


--------------------------------------------------------------------------------


-- DROP TABLE aggregated;
CREATE TABLE aggregated (
  id integer PRIMARY KEY,
  well_id integer NOT NULL REFERENCES wells,
  aggr_setting_id integer NOT NULL REFERENCES aggr_settings,
  parameter varchar (25) NOT NULL,
  value real NOT NULL,
  UNIQUE (well_id, aggr_setting_id)
);



--------------------------------------------------------------------------------


-- DROP TABLE disc_settings;
CREATE TABLE disc_settings (
  id integer PRIMARY KEY,
  plate_id integer NOT NULL REFERENCES plates,
  software varchar (25) NOT NULL,
  version varchar (25) NOT NULL,
  method varchar (25) NOT NULL,
  -- "options" is intended for a JSON or YAML dump of the option list
  options text NOT NULL
  -- a UNIQUE constraint would involve quite a few comparisons
);


--------------------------------------------------------------------------------


-- DROP TABLE discretized;
CREATE TABLE discretized (
  id integer PRIMARY KEY,
  well_id integer NOT NULL REFERENCES wells,
  disc_setting_id integer NOT NULL REFERENCES disc_settings,
  -- NULL is allowed because it means intermediate/ambiguous
  value boolean,
  UNIQUE (well_id, disc_setting_id)
);


--------------------------------------------------------------------------------

