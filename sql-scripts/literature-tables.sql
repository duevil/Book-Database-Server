DROP TABLE IF EXISTS books;
CREATE TABLE books
(
    id        SMALLINT PRIMARY KEY,
    title     VARCHAR(100) NOT NULL,
    publisher VARCHAR(100) NOT NULL,
    year      SMALLINT     NOT NULL,
    pages     SMALLINT     NOT NULL,
    keywords  VARCHAR(1000)
);

DROP TABLE IF EXISTS subfields;
CREATE TABLE subfields
(
    id   SMALLINT PRIMARY KEY,
    name varchar(100) NOT NULL
);

DROP TABLE IF EXISTS authors;
CREATE TABLE authors
(
    id         SMALLINT PRIMARY KEY,
    first_name VARCHAR(50) NOT NULL,
    last_name  VARCHAR(50) NOT NULL
);

DROP TABLE IF EXISTS is_author;
CREATE TABLE is_author
(
    author_id SMALLINT PRIMARY KEY REFERENCES authors (id),
    book_id   SMALLINT PRIMARY KEY REFERENCES books (id)
);

DROP TABLE IF EXISTS has_subfield;
CREATE TABLE has_subfield
(
    book_id     SMALLINT PRIMARY KEY REFERENCES books (id),
    subgenre_id SMALLINT PRIMARY KEY REFERENCES subfields (id)
)