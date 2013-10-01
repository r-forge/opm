

-- -----------------------------------------------------------------------------
--
-- Code for deleting all rows from the tables that hold PM data in the correct
-- order. Tested with PostgreSQL (9.1), SQLite (3.7.9) and MySQL (5.5.32).
--
-- -----------------------------------------------------------------------------

DELETE FROM discretized;
DELETE FROM disc_settings;

DELETE FROM aggregated;
DELETE FROM aggr_settings;

DELETE FROM measurements;

DELETE FROM wells;
DELETE FROM plates;

