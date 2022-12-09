DROP TABLE IF EXISTS books;
CREATE TABLE books
(
    id        SMALLINT PRIMARY KEY,
    title     VARCHAR(128) NOT NULL,
    publisher VARCHAR(128) NOT NULL,
    year      SMALLINT     NOT NULL,
    pages     SMALLINT     NOT NULL
);

DROP TABLE IF EXISTS subfields;
CREATE TABLE subfields
(
    id   SMALLINT PRIMARY KEY,
    name varchar(64) NOT NULL
);

DROP TABLE IF EXISTS authors;
CREATE TABLE authors
(
    id         SMALLINT PRIMARY KEY,
    first_name VARCHAR(64) NOT NULL,
    last_name  VARCHAR(64) NOT NULL
);

DROP TABLE IF EXISTS book_authors;
CREATE TABLE book_authors
(
    author_id SMALLINT REFERENCES authors (id),
    book_id   SMALLINT REFERENCES books (id),
    PRIMARY KEY (author_id, book_id)
);

DROP TABLE IF EXISTS book_subfields;
CREATE TABLE book_subfields
(
    book_id     SMALLINT REFERENCES books (id),
    subfield_id SMALLINT REFERENCES subfields (id),
    PRIMARY KEY (book_id, subfield_id)
);