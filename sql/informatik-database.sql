CREATE DATABASE IF NOT EXISTS informatik;

CREATE USER IF NOT EXISTS 'minf'@'localhost' IDENTIFIED BY 'prog3';

USE informatik;

GRANT ALL PRIVILEGES ON * TO 'minf'@'localhost';

-- TODO: add table
-- CREATE TABLE IF NOT EXISTS