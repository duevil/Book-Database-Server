CREATE SCHEMA IF NOT EXISTS informatik;
CREATE USER IF NOT EXISTS 'minf'@'localhost' IDENTIFIED BY 'prog3';
USE informatik;
GRANT ALL ON * TO 'minf'@'localhost' WITH GRANT OPTION;