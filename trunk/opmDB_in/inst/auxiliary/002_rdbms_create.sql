

-- -----------------------------------------------------------------------------
--
-- Code for creating the tables that hold PM data in the correct order. Tested
-- with PostgreSQL (9.1), SQLite (3.7.9) and MySQL (5.5.32).
--
-- -----------------------------------------------------------------------------


-- DROP TABLE IF EXISTS plates;
CREATE TABLE IF NOT EXISTS plates (
  id integer PRIMARY KEY,
  plate_type varchar (25) NOT NULL,
  -- one might need other datatype in other RDBMS, should cover date AND time:
  setup_time timestamp NOT NULL,
  -- string length does not vary (we assume standardisation):
  position character (4) NOT NULL,
  -- necessary if several instruments are involved:
  machine_id integer NOT NULL,
  -- this holds all originally read CSV data in JSON format for restoring them:
  csv_data text NOT NULL,
  -- per instrument, setup time and position identify each plate:
  UNIQUE (setup_time, position, machine_id)
);

-- NB: THIS TABLE MIGHT NEED ADDITIONS BY THE USER!
-- The metadata of interest, if any, should be added as columns to the 'plates'
-- table after the columns defined above and with appropriate data types. We do
-- not include metadata columns here because an OPMX object can contain any
-- combination of metadata.

-- -----------------------------------------------------------------------------


-- DROP TABLE IF EXISTS wells;
CREATE TABLE IF NOT EXISTS wells (
  id integer PRIMARY KEY,
  plate_id integer NOT NULL REFERENCES plates,
  -- string length does not vary (we assume standardisation):
  coordinate character (3) NOT NULL,
  UNIQUE (plate_id, coordinate)
);


-- -----------------------------------------------------------------------------


-- DROP TABLE IF EXISTS measurements;
CREATE TABLE IF NOT EXISTS measurements (
  id integer PRIMARY KEY,
  well_id integer NOT NULL REFERENCES wells,
  time real NOT NULL,
  value real NOT NULL,
  UNIQUE (well_id, time)
);


-- -----------------------------------------------------------------------------


-- DROP TABLE IF EXISTS aggr_settings;
CREATE TABLE IF NOT EXISTS aggr_settings (
  id integer PRIMARY KEY,
  plate_id integer NOT NULL REFERENCES plates,
  software varchar (25) NOT NULL,
  version varchar (25) NOT NULL,
  method varchar (25) NOT NULL,
  options text NOT NULL -- intended for a JSON dump of the option list
  -- a sensible UNIQUE constraint would involve quite a few comparisons here
);


-- -----------------------------------------------------------------------------


-- DROP TABLE IF EXISTS aggregated;
CREATE TABLE IF NOT EXISTS aggregated (
  id integer PRIMARY KEY,
  well_id integer NOT NULL REFERENCES wells,
  aggr_setting_id integer NOT NULL REFERENCES aggr_settings,
  parameter varchar (25) NOT NULL,
  value double precision NOT NULL,
  UNIQUE (well_id, aggr_setting_id, parameter)
);



-- -----------------------------------------------------------------------------


-- DROP TABLE IF EXISTS disc_settings;
CREATE TABLE IF NOT EXISTS disc_settings (
  id integer PRIMARY KEY,
  plate_id integer NOT NULL REFERENCES plates,
  software varchar (25) NOT NULL,
  version varchar (25) NOT NULL,
  method varchar (25) NOT NULL,
  options text NOT NULL -- intended for a JSON dump of the option list
  -- a sensible UNIQUE constraint would involve quite a few comparisons here
);


-- -----------------------------------------------------------------------------


-- DROP TABLE IF EXISTS discretized;
CREATE TABLE IF NOT EXISTS discretized (
  id integer PRIMARY KEY,
  well_id integer NOT NULL REFERENCES wells,
  disc_setting_id integer NOT NULL REFERENCES disc_settings,
  value boolean, -- NULL is allowed because it means intermediate/ambiguous
  UNIQUE (well_id, disc_setting_id)
);


-- -----------------------------------------------------------------------------


