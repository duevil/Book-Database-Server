DROP TABLE IF EXISTS books;
CREATE TABLE books
(
    title     VARCHAR(128) PRIMARY KEY,
    publisher VARCHAR(128) NOT NULL,
    year      SMALLINT     NOT NULL,
    pages     SMALLINT     NOT NULL
);

DROP TABLE IF EXISTS subfields;
CREATE TABLE subfields
(
    name VARCHAR(64) PRIMARY KEY
);

DROP TABLE IF EXISTS authors;
CREATE TABLE authors
(
    first_name VARCHAR(64),
    last_name  VARCHAR(64),
    PRIMARY KEY (first_name, last_name)
);

DROP TABLE IF EXISTS book_authors;
CREATE TABLE book_authors
(
    book VARCHAR(128) REFERENCES books(title),
    author_first_name VARCHAR(64) REFERENCES authors(first_name),
    author_last_name VARCHAR(64) REFERENCES authors(last_name),
    PRIMARY KEY (book, author_first_name, author_last_name)
);

DROP TABLE IF EXISTS book_subfields;
CREATE TABLE book_subfields
(
    book VARCHAR(128) REFERENCES books(title),
    subfield VARCHAR(64) REFERENCES subfields(name),
    PRIMARY KEY (book, subfield)
);