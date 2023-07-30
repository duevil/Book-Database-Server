# noinspection SpellCheckingInspectionForFile
# (C) Malte Kasolowsky 2023

USE informatik;

# ------ table books ------

DROP TABLE IF EXISTS books;
CREATE TABLE books
(
    id        SMALLINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    title     VARCHAR(128) NOT NULL,
    publisher VARCHAR(128) NOT NULL,
    year      SMALLINT     NOT NULL,
    pages     SMALLINT     NOT NULL,
    rating    SMALLINT     NOT NULL
);

# ------ table subfields ------

DROP TABLE IF EXISTS subfields;
CREATE TABLE subfields
(
    id   SMALLINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    name varchar(64) NOT NULL
);

# ------ table authors ------

DROP TABLE IF EXISTS authors;
CREATE TABLE authors
(
    id         SMALLINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    first_name VARCHAR(64) NOT NULL,
    last_name  VARCHAR(64) NOT NULL,
    UNIQUE (first_name, last_name)
);

# ------ table book authors ------

DROP TABLE IF EXISTS book_authors;
CREATE TABLE book_authors
(
    author_id SMALLINT REFERENCES authors (id),
    book_id   SMALLINT REFERENCES books (id),
    PRIMARY KEY (author_id, book_id)
);

# ------ table book subfields ------

DROP TABLE IF EXISTS book_subfields;
CREATE TABLE book_subfields
(
    book_id     SMALLINT REFERENCES books (id),
    subfield_id SMALLINT REFERENCES subfields (id),
    PRIMARY KEY (book_id, subfield_id)
);


# ------ data subfields ------ 

INSERT INTO subfields (id, name)
VALUES (1, 'Artificial intelligence');

INSERT INTO subfields (id, name)
VALUES (2, 'Programming languages and logic');

INSERT INTO subfields (id, name)
VALUES (3, 'Scientific computing applications');

INSERT INTO subfields (id, name)
VALUES (4, 'Theory of computation');

INSERT INTO subfields (id, name)
VALUES (5, 'Data structures and algorithms');

INSERT INTO subfields (id, name)
VALUES (6, 'Computer architecture and organization');

INSERT INTO subfields (id, name)
VALUES (7, 'Computer networks');

INSERT INTO subfields (id, name)
VALUES (8, 'Computer security in cryptography');

INSERT INTO subfields (id, name)
VALUES (9, 'Databases and data mining');

INSERT INTO subfields (id, name)
VALUES (10, 'Computer graphics and visualization');

INSERT INTO subfields (id, name)
VALUES (11, 'Image and sound processing');

INSERT INTO subfields (id, name)
VALUES (12, 'Concurrent, parallel and distributed computing');

INSERT INTO subfields (id, name)
VALUES (13, 'Human-computer interaction');

INSERT INTO subfields (id, name)
VALUES (14, 'Software engineering');

INSERT INTO subfields (id, name)
VALUES (15, 'Information and coding theory');


# ------ data books ------ 


# ---- data book nr. 001 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (1, 2, 'Reed Publishing', 177, 2010, 'Introduction to Parallel and Distributed Computing');

# ---- data book nr. 001 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Glenn', 1, 'Reichert');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (1, 1);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Inga', 3, 'Renner');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (3, 1);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Nestor', 6, 'Bayer');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (6, 1);

# -- data author nr. 004 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Shelton', 5, 'Ward');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (5, 1);

# -- data author nr. 005 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Starla', 2, 'Medhurst');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (2, 1);

# -- data author nr. 006 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Tommy', 4, 'Rice');

# -- data book author nr. 006 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (4, 1);

# ---- data book nr. 001 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (1, 8);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (1, 1);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (1, 12);


# ---- data book nr. 002 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (2, 3, 'University of Nebraska Press', 267, 1998,
        'The Computer Science of Psychology: How Computers Can Help You Understand Mental Health');

# ---- data book nr. 002 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Hector', 11, 'Johnston');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (11, 2);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Jesusa', 9, 'Stiedemann');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (9, 2);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Michel', 10, 'Ullrich');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (10, 2);

# -- data author nr. 004 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Shin', 7, 'Feeney');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (7, 2);

# -- data author nr. 005 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Werner', 8, 'Zieme');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (8, 2);

# ---- data book nr. 002 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (2, 3);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (2, 10);


# ---- data book nr. 003 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (3, 5, 'Etruscan Press', 286, 2017, 'Programming the Amazon: A Beginner''s Guide to Programming the AWS Cloud');

# ---- data book nr. 003 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Bonnie', 14, 'Windler');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (14, 3);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Kimi', 12, 'Mraz');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (12, 3);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Shayna', 13, 'Harris');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (13, 3);

# ---- data book nr. 003 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (3, 4);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (3, 11);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (3, 2);

# -- book subfield nr. 004 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (3, 9);


# ---- data book nr. 004 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (4, 1, 'G-Unit Books', 400, 1980,
        'Data Structures and Algorithms: A Hands-On Introduction to the Theory and Practice');

# ---- data book nr. 004 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Dana', 16, 'Nader');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (16, 4);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Davis', 17, 'Hettinger');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (17, 4);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Devorah', 20, 'Zieme');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (20, 4);

# -- data author nr. 004 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Franklyn', 19, 'Shields');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (19, 4);

# -- data author nr. 005 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Ginette', 18, 'Boyle');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (18, 4);

# -- data author nr. 006 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Sherry', 15, 'Kovacek');

# -- data book author nr. 006 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (15, 4);

# ---- data book nr. 004 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (4, 5);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (4, 8);


# ---- data book nr. 005 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (5, 5, 'Pavilion Books', 282, 1978, 'A guide to Computer Science, 4th Edition');

# ---- data book nr. 005 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Bryce', 23, 'Cartwright');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (23, 5);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Soila', 21, 'Hettinger');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (21, 5);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Tamatha', 22, 'Pfeffer');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (22, 5);

# ---- data book nr. 005 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (5, 3);


# ---- data book nr. 006 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (6, 1, 'New Holland Publishers', 438, 1954,
        'Efficient Data Processing: Advanced Computer Vision for Scientists and Engineers');

# ---- data book nr. 006 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Alberta', 26, 'Balistreri');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (26, 6);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Christy', 28, 'Hilll');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (28, 6);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Fredric', 29, 'Ritchie');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (29, 6);

# -- data author nr. 004 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Leonardo', 24, 'Heidenreich');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (24, 6);

# -- data author nr. 005 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Leora', 25, 'Leuschke');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (25, 6);

# -- data author nr. 006 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Melynda', 27, 'Klein');

# -- data book author nr. 006 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (27, 6);

# ---- data book nr. 006 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (6, 3);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (6, 11);


# ---- data book nr. 007 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (7, 4, 'Peace Hill Press', 477, 1989,
        'Programming the human mind: Cognitive science and how it will change what you can do with computers');

# ---- data book nr. 007 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Barton', 31, 'Bins');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (31, 7);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Jayna', 30, 'Stoltenberg');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (30, 7);

# ---- data book nr. 007 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (7, 14);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (7, 13);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (7, 1);


# ---- data book nr. 008 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (8, 1, 'George Routledge & Sons', 38, 2021, 'Images and Sounds of the Computer World');

# ---- data book nr. 008 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Denny', 32, 'Wilderman');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (32, 8);

# ---- data book nr. 008 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (8, 11);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (8, 3);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (8, 14);


# ---- data book nr. 009 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (9, 2, 'Canongate Books', 170, 2013, 'Computer Vision: From Pattern Recognition to Machine Learning');

# ---- data book nr. 009 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Blake', 33, 'Borer');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (33, 9);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Felipe', 34, 'Nitzsche');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (34, 9);

# ---- data book nr. 009 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (9, 1);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (9, 10);


# ---- data book nr. 010 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (10, 4, 'Kodansha', 113, 2013, 'The Visual Computer: How to Build Better Images, Animations, and Games');

# ---- data book nr. 010 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Adalberto', 36, 'Gulgowski');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (36, 10);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Aldo', 37, 'Haley');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (37, 10);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Donald', 35, 'Konopelski');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (35, 10);

# ---- data book nr. 010 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (10, 11);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (10, 13);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (10, 12);


# ---- data book nr. 011 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (11, 5, 'Del Rey Books', 492, 2022, 'Parallel Programming: Accelerating Science, Engineering, and Technology');

# ---- data book nr. 011 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Bryan', 38, 'Wehner');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (38, 11);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Dede', 40, 'Kerluke');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (40, 11);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Lorine', 39, 'Ledner');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (39, 11);

# ---- data book nr. 011 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (11, 3);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (11, 7);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (11, 12);


# ---- data book nr. 012 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (12, 2, 'ECW Press', 347, 1986, 'Embedded Systems');

# ---- data book nr. 012 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Paris', 41, 'VonRueden');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (41, 12);

# ---- data book nr. 012 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (12, 5);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (12, 15);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (12, 7);


# ---- data book nr. 013 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (13, 5, 'Legend Books', 12, 1956, 'Computational Thinking: Using Data, Graphics, and Math to Solve Problems');

# ---- data book nr. 013 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Dean', 42, 'Sporer');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (42, 13);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Tiana', 43, 'Kling');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (43, 13);

# ---- data book nr. 013 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (13, 15);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (13, 12);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (13, 10);

# -- book subfield nr. 004 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (13, 9);


# ---- data book nr. 014 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (14, 5, 'Chick Publications', 160, 1974, 'Cracking the Code');

# ---- data book nr. 014 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Rickie', 44, 'Wintheiser');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (44, 14);

# ---- data book nr. 014 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (14, 8);


# ---- data book nr. 015 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (15, 1, 'Scholastic Press', 450, 1980,
        'Artificial Intelligence: Powerful New Ideas for Understanding the World');

# ---- data book nr. 015 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Emmett', 45, 'Runte');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (45, 15);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Ernestine', 49, 'Hahn');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (49, 15);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Hortense', 47, 'Bogan');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (47, 15);

# -- data author nr. 004 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Jordan', 50, 'Kautzer');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (50, 15);

# -- data author nr. 005 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Kirstie', 46, 'Bernhard');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (46, 15);

# -- data author nr. 006 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Mellisa', 48, 'Heathcote');

# -- data book author nr. 006 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (48, 15);

# ---- data book nr. 015 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (15, 15);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (15, 12);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (15, 11);

# -- book subfield nr. 004 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (15, 1);


# ---- data book nr. 016 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (16, 2, 'Faber and Faber', 234, 2020, 'Data Mining: Practical Machine Learning Tools and Techniques');

# ---- data book nr. 016 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Ammie', 52, 'Stoltenberg');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (52, 16);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Anna', 54, 'Braun');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (54, 16);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('German', 51, 'Reichert');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (51, 16);

# -- data author nr. 004 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Jacalyn', 53, 'Bogisich');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (53, 16);

# ---- data book nr. 016 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (16, 4);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (16, 11);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (16, 9);

# -- book subfield nr. 004 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (16, 1);


# ---- data book nr. 017 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (17, 2, 'Happy House', 310, 2004, 'Case Study: The Face Recognition Problem');

# ---- data book nr. 017 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Angie', 59, 'Yundt');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (59, 17);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Forrest', 57, 'Okuneva');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (57, 17);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Gonzalo', 58, 'Schultz');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (58, 17);

# -- data author nr. 004 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Myesha', 56, 'Kemmer');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (56, 17);

# -- data author nr. 005 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Reita', 60, 'Senger');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (60, 17);

# -- data author nr. 006 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Sook', 55, 'Dare');

# -- data book author nr. 006 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (55, 17);

# ---- data book nr. 017 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (17, 1);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (17, 10);


# ---- data book nr. 018 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (18, 3, 'Parachute Publishing', 259, 1987, 'Hardware/Software Interfaces');

# ---- data book nr. 018 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Coralie', 61, 'Green');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (61, 18);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Eugene', 62, 'Lynch');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (62, 18);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Kimiko', 63, 'Blick');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (63, 18);

# ---- data book nr. 018 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (18, 6);


# ---- data book nr. 019 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (19, 2, 'Boydell & Brewer', 60, 2004, 'The Little Book of Concurrency');

# ---- data book nr. 019 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Carolin', 66, 'Okuneva');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (66, 19);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Katie', 64, 'Labadie');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (64, 19);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Lynelle', 65, 'Beatty');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (65, 19);

# -- data author nr. 004 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Thad', 67, 'Roob');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (67, 19);

# ---- data book nr. 019 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (19, 12);


# ---- data book nr. 020 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (20, 5, 'Medknow Publications', 101, 2022, 'Information Retrieval, Search Engines, and Web Crawlers');

# ---- data book nr. 020 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Chance', 71, 'Stokes');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (71, 20);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Dione', 73, 'Doyle');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (73, 20);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Harvey', 68, 'Okuneva');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (68, 20);

# -- data author nr. 004 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Reynaldo', 69, 'Nicolas');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (69, 20);

# -- data author nr. 005 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Ruthe', 72, 'Douglas');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (72, 20);

# -- data author nr. 006 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Tonisha', 70, 'Johns');

# -- data book author nr. 006 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (70, 20);

# ---- data book nr. 020 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (20, 6);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (20, 13);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (20, 3);

# -- book subfield nr. 004 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (20, 9);


# ---- data book nr. 021 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (21, 1, 'Hachette Book Group USA', 91, 1991, 'Abstract Algebra: The Language');

# ---- data book nr. 021 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Lorena', 74, 'Fahey');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (74, 21);

# ---- data book nr. 021 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (21, 15);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (21, 11);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (21, 3);

# -- book subfield nr. 004 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (21, 2);


# ---- data book nr. 022 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (22, 2, 'Gaspereau Press', 63, 2010, 'Simplified Algorithms: An Introduction to Data Structures and Algorithms');

# ---- data book nr. 022 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Mercy', 76, 'Russel');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (76, 22);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Norene', 75, 'Harber');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (75, 22);

# ---- data book nr. 022 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (22, 5);


# ---- data book nr. 023 ---- 

INSERT INTO books (id, rating, publisher, pages, year, title)
VALUES (23, 2, 'Voyager Books', 299, 2017,
        'How to Build a Computer That Automatically Dooms the Disposable Population to a Life of Serenity');

# ---- data book nr. 023 authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Leila', 79, 'Deckow');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (79, 23);

# -- data author nr. 002 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Reed', 77, 'Muller');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (77, 23);

# -- data author nr. 003 -- 
INSERT INTO authors (first_name, id, last_name)
VALUES ('Sheryll', 78, 'Leuschke');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (author_id, book_id)
VALUES (78, 23);

# ---- data book nr. 023 subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (23, 5);

# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (23, 14);

# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (book_id, subfield_id)
VALUES (23, 1);

