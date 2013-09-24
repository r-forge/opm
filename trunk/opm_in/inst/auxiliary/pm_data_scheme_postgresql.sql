

--------------------------------------------------------------------------------
--
-- Code for creating the tables that hold PM data. Tested with PostgreSQL 9.1.
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
  -- per instrument, setup time and position identify each plate:
  UNIQUE (setup_time, position, machine_id),
  -- this holds all originally read CSV data in JSON format for restoring them:
  csv_data text NOT NULL
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
  -- using a REAL is slightly overkill; decimal points are usually zero
  time real NOT NULL,
  value real NOT NULL,
  UNIQUE (well_id, time)
);


--------------------------------------------------------------------------------


