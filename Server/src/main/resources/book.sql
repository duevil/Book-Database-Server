# noinspection SpellCheckingInspectionForFile
# (C) Malte Kasolowsky 2023

USE informatik;

# ------ table books ------

DROP TABLE IF EXISTS books;
CREATE TABLE books
(
    title     VARCHAR(128) PRIMARY KEY,
    publisher VARCHAR(128),
    year      SMALLINT UNSIGNED,
    pages     SMALLINT UNSIGNED,
    rating    SMALLINT UNSIGNED DEFAULT 3
);

# ------ table authors ------

DROP TABLE IF EXISTS authors;
CREATE TABLE authors
(
    first_name VARCHAR(64),
    last_name  VARCHAR(64),
    PRIMARY KEY (first_name, last_name)
);

# ------ table subfields ------

DROP TABLE IF EXISTS subfields;
CREATE TABLE subfields
(
    name VARCHAR(64) PRIMARY KEY
);

# ------ table book autors ------

DROP TABLE IF EXISTS book_authors;
CREATE TABLE book_authors
(
    book              VARCHAR(128) REFERENCES books (title),
    author_first_name VARCHAR(64) REFERENCES authors (first_name),
    author_last_name  VARCHAR(64) REFERENCES authors (last_name),
    PRIMARY KEY (book, author_first_name, author_last_name)
);

# ------ table book subfields ------

DROP TABLE IF EXISTS book_subfields;
CREATE TABLE book_subfields
(
    book     VARCHAR(128) REFERENCES books (title),
    subfield VARCHAR(64) REFERENCES subfields (name),
    PRIMARY KEY (book, subfield)
);


# ------ data subfields ------ 

INSERT INTO subfields (name)
VALUES ('Artificial intelligence');

INSERT INTO subfields (name)
VALUES ('Programming languages and logic');

INSERT INTO subfields (name)
VALUES ('Scientific computing applications');

INSERT INTO subfields (name)
VALUES ('Theory of computation');

INSERT INTO subfields (name)
VALUES ('Data structures and algorithms');

INSERT INTO subfields (name)
VALUES ('Computer architecture and organization');

INSERT INTO subfields (name)
VALUES ('Computer networks');

INSERT INTO subfields (name)
VALUES ('Computer security in cryptography');

INSERT INTO subfields (name)
VALUES ('Databases and data mining');

INSERT INTO subfields (name)
VALUES ('Computer graphics and visualization');

INSERT INTO subfields (name)
VALUES ('Image and sound processing');

INSERT INTO subfields (name)
VALUES ('Concurrent, parallel and distributed computing');

INSERT INTO subfields (name)
VALUES ('Human-computer interaction');

INSERT INTO subfields (name)
VALUES ('Software engineering');

INSERT INTO subfields (name)
VALUES ('Information and coding theory');


# ------ data books ------ 

# ---- data book nr. 001 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (1956, 1, 12, 'Legend Books', 'Computational Thinking: Using Data, Graphics, and Math to Solve Problems');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Kling', 'Tiana');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Computational Thinking: Using Data, Graphics, and Math to Solve Problems', 'Kling', 'Tiana');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Sporer', 'Dean');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Computational Thinking: Using Data, Graphics, and Math to Solve Problems', 'Sporer', 'Dean');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Computer graphics and visualization',
        'Computational Thinking: Using Data, Graphics, and Math to Solve Problems');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Concurrent, parallel and distributed computing',
        'Computational Thinking: Using Data, Graphics, and Math to Solve Problems');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Information and coding theory', 'Computational Thinking: Using Data, Graphics, and Math to Solve Problems');


# -- book subfield nr. 004 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Databases and data mining', 'Computational Thinking: Using Data, Graphics, and Math to Solve Problems');


# ---- data book nr. 002 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (2021, 2, 38, 'George Routledge & Sons', 'Images and Sounds of the Computer World');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Wilderman', 'Denny');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Images and Sounds of the Computer World', 'Wilderman', 'Denny');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Software engineering', 'Images and Sounds of the Computer World');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Image and sound processing', 'Images and Sounds of the Computer World');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Scientific computing applications', 'Images and Sounds of the Computer World');


# ---- data book nr. 003 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (2017, 3, 286, 'Etruscan Press', 'Programming the Amazon: A Beginner''s Guide to Programming the AWS Cloud');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Mraz', 'Kimi');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Programming the Amazon: A Beginner''s Guide to Programming the AWS Cloud', 'Mraz', 'Kimi');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Harris', 'Shayna');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Programming the Amazon: A Beginner''s Guide to Programming the AWS Cloud', 'Harris', 'Shayna');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Windler', 'Bonnie');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Programming the Amazon: A Beginner''s Guide to Programming the AWS Cloud', 'Windler', 'Bonnie');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Image and sound processing', 'Programming the Amazon: A Beginner''s Guide to Programming the AWS Cloud');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Programming languages and logic', 'Programming the Amazon: A Beginner''s Guide to Programming the AWS Cloud');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Theory of computation', 'Programming the Amazon: A Beginner''s Guide to Programming the AWS Cloud');


# -- book subfield nr. 004 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Databases and data mining', 'Programming the Amazon: A Beginner''s Guide to Programming the AWS Cloud');


# ---- data book nr. 004 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (1987, 2, 259, 'Parachute Publishing', 'Hardware/Software Interfaces');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Green', 'Coralie');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Hardware/Software Interfaces', 'Green', 'Coralie');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Blick', 'Kimiko');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Hardware/Software Interfaces', 'Blick', 'Kimiko');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Lynch', 'Eugene');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Hardware/Software Interfaces', 'Lynch', 'Eugene');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Computer architecture and organization', 'Hardware/Software Interfaces');


# ---- data book nr. 005 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (1989, 4, 477, 'Peace Hill Press',
        'Programming the human mind: Cognitive science and how it will change what you can do with computers');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Bins', 'Barton');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Programming the human mind: Cognitive science and how it will change what you can do with computers', 'Bins',
        'Barton');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Stoltenberg', 'Jayna');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Programming the human mind: Cognitive science and how it will change what you can do with computers',
        'Stoltenberg', 'Jayna');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Artificial intelligence',
        'Programming the human mind: Cognitive science and how it will change what you can do with computers');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Human-computer interaction',
        'Programming the human mind: Cognitive science and how it will change what you can do with computers');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Software engineering',
        'Programming the human mind: Cognitive science and how it will change what you can do with computers');


# ---- data book nr. 006 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (2013, 5, 113, 'Kodansha', 'The Visual Computer: How to Build Better Images, Animations, and Games');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Konopelski', 'Donald');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('The Visual Computer: How to Build Better Images, Animations, and Games', 'Konopelski', 'Donald');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Gulgowski', 'Adalberto');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('The Visual Computer: How to Build Better Images, Animations, and Games', 'Gulgowski', 'Adalberto');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Haley', 'Aldo');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('The Visual Computer: How to Build Better Images, Animations, and Games', 'Haley', 'Aldo');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Human-computer interaction', 'The Visual Computer: How to Build Better Images, Animations, and Games');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Image and sound processing', 'The Visual Computer: How to Build Better Images, Animations, and Games');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Concurrent, parallel and distributed computing',
        'The Visual Computer: How to Build Better Images, Animations, and Games');


# ---- data book nr. 007 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (2013, 2, 170, 'Canongate Books', 'Computer Vision: From Pattern Recognition to Machine Learning');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Nitzsche', 'Felipe');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Computer Vision: From Pattern Recognition to Machine Learning', 'Nitzsche', 'Felipe');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Borer', 'Blake');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Computer Vision: From Pattern Recognition to Machine Learning', 'Borer', 'Blake');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Computer graphics and visualization', 'Computer Vision: From Pattern Recognition to Machine Learning');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Artificial intelligence', 'Computer Vision: From Pattern Recognition to Machine Learning');


# ---- data book nr. 008 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (1974, 3, 160, 'Chick Publications', 'Cracking the Code');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Wintheiser', 'Rickie');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Cracking the Code', 'Wintheiser', 'Rickie');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Computer security in cryptography', 'Cracking the Code');


# ---- data book nr. 009 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (2004, 4, 60, 'Boydell & Brewer', 'The Little Book of Concurrency');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Roob', 'Thad');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('The Little Book of Concurrency', 'Roob', 'Thad');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Okuneva', 'Carolin');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('The Little Book of Concurrency', 'Okuneva', 'Carolin');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Labadie', 'Katie');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('The Little Book of Concurrency', 'Labadie', 'Katie');


# -- data author nr. 004 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Beatty', 'Lynelle');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('The Little Book of Concurrency', 'Beatty', 'Lynelle');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Concurrent, parallel and distributed computing', 'The Little Book of Concurrency');


# ---- data book nr. 010 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (2020, 3, 234, 'Faber and Faber', 'Data Mining: Practical Machine Learning Tools and Techniques');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Reichert', 'German');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Data Mining: Practical Machine Learning Tools and Techniques', 'Reichert', 'German');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Bogisich', 'Jacalyn');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Data Mining: Practical Machine Learning Tools and Techniques', 'Bogisich', 'Jacalyn');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Stoltenberg', 'Ammie');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Data Mining: Practical Machine Learning Tools and Techniques', 'Stoltenberg', 'Ammie');


# -- data author nr. 004 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Braun', 'Anna');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Data Mining: Practical Machine Learning Tools and Techniques', 'Braun', 'Anna');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Image and sound processing', 'Data Mining: Practical Machine Learning Tools and Techniques');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Theory of computation', 'Data Mining: Practical Machine Learning Tools and Techniques');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Artificial intelligence', 'Data Mining: Practical Machine Learning Tools and Techniques');


# -- book subfield nr. 004 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Databases and data mining', 'Data Mining: Practical Machine Learning Tools and Techniques');


# ---- data book nr. 011 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (2022, 2, 101, 'Medknow Publications', 'Information Retrieval, Search Engines, and Web Crawlers');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Okuneva', 'Harvey');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Information Retrieval, Search Engines, and Web Crawlers', 'Okuneva', 'Harvey');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Doyle', 'Dione');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Information Retrieval, Search Engines, and Web Crawlers', 'Doyle', 'Dione');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Stokes', 'Chance');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Information Retrieval, Search Engines, and Web Crawlers', 'Stokes', 'Chance');


# -- data author nr. 004 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Johns', 'Tonisha');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Information Retrieval, Search Engines, and Web Crawlers', 'Johns', 'Tonisha');


# -- data author nr. 005 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Nicolas', 'Reynaldo');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Information Retrieval, Search Engines, and Web Crawlers', 'Nicolas', 'Reynaldo');


# -- data author nr. 006 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Douglas', 'Ruthe');

# -- data book author nr. 006 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Information Retrieval, Search Engines, and Web Crawlers', 'Douglas', 'Ruthe');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Scientific computing applications', 'Information Retrieval, Search Engines, and Web Crawlers');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Human-computer interaction', 'Information Retrieval, Search Engines, and Web Crawlers');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Databases and data mining', 'Information Retrieval, Search Engines, and Web Crawlers');


# -- book subfield nr. 004 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Computer architecture and organization', 'Information Retrieval, Search Engines, and Web Crawlers');


# ---- data book nr. 012 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (1954, 2, 438, 'New Holland Publishers',
        'Efficient Data Processing: Advanced Computer Vision for Scientists and Engineers');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Hilll', 'Christy');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Efficient Data Processing: Advanced Computer Vision for Scientists and Engineers', 'Hilll', 'Christy');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Balistreri', 'Alberta');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Efficient Data Processing: Advanced Computer Vision for Scientists and Engineers', 'Balistreri', 'Alberta');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Ritchie', 'Fredric');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Efficient Data Processing: Advanced Computer Vision for Scientists and Engineers', 'Ritchie', 'Fredric');


# -- data author nr. 004 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Klein', 'Melynda');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Efficient Data Processing: Advanced Computer Vision for Scientists and Engineers', 'Klein', 'Melynda');


# -- data author nr. 005 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Heidenreich', 'Leonardo');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Efficient Data Processing: Advanced Computer Vision for Scientists and Engineers', 'Heidenreich', 'Leonardo');


# -- data author nr. 006 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Leuschke', 'Leora');

# -- data book author nr. 006 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Efficient Data Processing: Advanced Computer Vision for Scientists and Engineers', 'Leuschke', 'Leora');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Image and sound processing',
        'Efficient Data Processing: Advanced Computer Vision for Scientists and Engineers');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Scientific computing applications',
        'Efficient Data Processing: Advanced Computer Vision for Scientists and Engineers');


# ---- data book nr. 013 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (2010, 5, 63, 'Gaspereau Press', 'Simplified Algorithms: An Introduction to Data Structures and Algorithms');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Harber', 'Norene');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Simplified Algorithms: An Introduction to Data Structures and Algorithms', 'Harber', 'Norene');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Russel', 'Mercy');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Simplified Algorithms: An Introduction to Data Structures and Algorithms', 'Russel', 'Mercy');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Data structures and algorithms', 'Simplified Algorithms: An Introduction to Data Structures and Algorithms');


# ---- data book nr. 014 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (1978, 5, 282, 'Pavilion Books', 'A guide to Computer Science, 4th Edition');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Cartwright', 'Bryce');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('A guide to Computer Science, 4th Edition', 'Cartwright', 'Bryce');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Pfeffer', 'Tamatha');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('A guide to Computer Science, 4th Edition', 'Pfeffer', 'Tamatha');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Hettinger', 'Soila');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('A guide to Computer Science, 4th Edition', 'Hettinger', 'Soila');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Scientific computing applications', 'A guide to Computer Science, 4th Edition');


# ---- data book nr. 015 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (2017, 3, 299, 'Voyager Books',
        'How to Build a Computer That Automatically Dooms the Disposable Population to a Life of Serenity');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Leuschke', 'Sheryll');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('How to Build a Computer That Automatically Dooms the Disposable Population to a Life of Serenity', 'Leuschke',
        'Sheryll');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Muller', 'Reed');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('How to Build a Computer That Automatically Dooms the Disposable Population to a Life of Serenity', 'Muller',
        'Reed');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Deckow', 'Leila');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('How to Build a Computer That Automatically Dooms the Disposable Population to a Life of Serenity', 'Deckow',
        'Leila');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Software engineering',
        'How to Build a Computer That Automatically Dooms the Disposable Population to a Life of Serenity');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Artificial intelligence',
        'How to Build a Computer That Automatically Dooms the Disposable Population to a Life of Serenity');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Data structures and algorithms',
        'How to Build a Computer That Automatically Dooms the Disposable Population to a Life of Serenity');


# ---- data book nr. 016 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (2010, 3, 177, 'Reed Publishing', 'Introduction to Parallel and Distributed Computing');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Renner', 'Inga');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Introduction to Parallel and Distributed Computing', 'Renner', 'Inga');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Ward', 'Shelton');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Introduction to Parallel and Distributed Computing', 'Ward', 'Shelton');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Medhurst', 'Starla');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Introduction to Parallel and Distributed Computing', 'Medhurst', 'Starla');


# -- data author nr. 004 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Rice', 'Tommy');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Introduction to Parallel and Distributed Computing', 'Rice', 'Tommy');


# -- data author nr. 005 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Reichert', 'Glenn');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Introduction to Parallel and Distributed Computing', 'Reichert', 'Glenn');


# -- data author nr. 006 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Bayer', 'Nestor');

# -- data book author nr. 006 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Introduction to Parallel and Distributed Computing', 'Bayer', 'Nestor');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Artificial intelligence', 'Introduction to Parallel and Distributed Computing');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Concurrent, parallel and distributed computing', 'Introduction to Parallel and Distributed Computing');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Computer security in cryptography', 'Introduction to Parallel and Distributed Computing');


# ---- data book nr. 017 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (2004, 1, 310, 'Happy House', 'Case Study: The Face Recognition Problem');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Dare', 'Sook');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Case Study: The Face Recognition Problem', 'Dare', 'Sook');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Yundt', 'Angie');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Case Study: The Face Recognition Problem', 'Yundt', 'Angie');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Senger', 'Reita');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Case Study: The Face Recognition Problem', 'Senger', 'Reita');


# -- data author nr. 004 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Okuneva', 'Forrest');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Case Study: The Face Recognition Problem', 'Okuneva', 'Forrest');


# -- data author nr. 005 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Schultz', 'Gonzalo');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Case Study: The Face Recognition Problem', 'Schultz', 'Gonzalo');


# -- data author nr. 006 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Kemmer', 'Myesha');

# -- data book author nr. 006 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Case Study: The Face Recognition Problem', 'Kemmer', 'Myesha');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Computer graphics and visualization', 'Case Study: The Face Recognition Problem');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Artificial intelligence', 'Case Study: The Face Recognition Problem');


# ---- data book nr. 018 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (1991, 3, 91, 'Hachette Book Group USA', 'Abstract Algebra: The Language');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Fahey', 'Lorena');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Abstract Algebra: The Language', 'Fahey', 'Lorena');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Image and sound processing', 'Abstract Algebra: The Language');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Programming languages and logic', 'Abstract Algebra: The Language');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Scientific computing applications', 'Abstract Algebra: The Language');


# -- book subfield nr. 004 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Information and coding theory', 'Abstract Algebra: The Language');


# ---- data book nr. 019 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (1998, 3, 267, 'University of Nebraska Press',
        'The Computer Science of Psychology: How Computers Can Help You Understand Mental Health');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Stiedemann', 'Jesusa');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('The Computer Science of Psychology: How Computers Can Help You Understand Mental Health', 'Stiedemann',
        'Jesusa');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Feeney', 'Shin');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('The Computer Science of Psychology: How Computers Can Help You Understand Mental Health', 'Feeney', 'Shin');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Zieme', 'Werner');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('The Computer Science of Psychology: How Computers Can Help You Understand Mental Health', 'Zieme', 'Werner');


# -- data author nr. 004 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Ullrich', 'Michel');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('The Computer Science of Psychology: How Computers Can Help You Understand Mental Health', 'Ullrich', 'Michel');


# -- data author nr. 005 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Johnston', 'Hector');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('The Computer Science of Psychology: How Computers Can Help You Understand Mental Health', 'Johnston',
        'Hector');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Computer graphics and visualization',
        'The Computer Science of Psychology: How Computers Can Help You Understand Mental Health');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Scientific computing applications',
        'The Computer Science of Psychology: How Computers Can Help You Understand Mental Health');


# ---- data book nr. 020 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (1980, 3, 400, 'G-Unit Books',
        'Data Structures and Algorithms: A Hands-On Introduction to the Theory and Practice');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Shields', 'Franklyn');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Data Structures and Algorithms: A Hands-On Introduction to the Theory and Practice', 'Shields', 'Franklyn');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Nader', 'Dana');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Data Structures and Algorithms: A Hands-On Introduction to the Theory and Practice', 'Nader', 'Dana');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Kovacek', 'Sherry');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Data Structures and Algorithms: A Hands-On Introduction to the Theory and Practice', 'Kovacek', 'Sherry');


# -- data author nr. 004 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Boyle', 'Ginette');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Data Structures and Algorithms: A Hands-On Introduction to the Theory and Practice', 'Boyle', 'Ginette');


# -- data author nr. 005 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Hettinger', 'Davis');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Data Structures and Algorithms: A Hands-On Introduction to the Theory and Practice', 'Hettinger', 'Davis');


# -- data author nr. 006 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Zieme', 'Devorah');

# -- data book author nr. 006 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Data Structures and Algorithms: A Hands-On Introduction to the Theory and Practice', 'Zieme', 'Devorah');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Computer security in cryptography',
        'Data Structures and Algorithms: A Hands-On Introduction to the Theory and Practice');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Data structures and algorithms',
        'Data Structures and Algorithms: A Hands-On Introduction to the Theory and Practice');


# ---- data book nr. 021 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (1986, 3, 347, 'ECW Press', 'Embedded Systems');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('VonRueden', 'Paris');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Embedded Systems', 'VonRueden', 'Paris');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Information and coding theory', 'Embedded Systems');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Data structures and algorithms', 'Embedded Systems');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Computer networks', 'Embedded Systems');


# ---- data book nr. 022 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (1980, 2, 450, 'Scholastic Press', 'Artificial Intelligence: Powerful New Ideas for Understanding the World');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Heathcote', 'Mellisa');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Artificial Intelligence: Powerful New Ideas for Understanding the World', 'Heathcote', 'Mellisa');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Kautzer', 'Jordan');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Artificial Intelligence: Powerful New Ideas for Understanding the World', 'Kautzer', 'Jordan');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Runte', 'Emmett');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Artificial Intelligence: Powerful New Ideas for Understanding the World', 'Runte', 'Emmett');


# -- data author nr. 004 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Bogan', 'Hortense');

# -- data book author nr. 004 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Artificial Intelligence: Powerful New Ideas for Understanding the World', 'Bogan', 'Hortense');


# -- data author nr. 005 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Bernhard', 'Kirstie');

# -- data book author nr. 005 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Artificial Intelligence: Powerful New Ideas for Understanding the World', 'Bernhard', 'Kirstie');


# -- data author nr. 006 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Hahn', 'Ernestine');

# -- data book author nr. 006 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Artificial Intelligence: Powerful New Ideas for Understanding the World', 'Hahn', 'Ernestine');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Image and sound processing', 'Artificial Intelligence: Powerful New Ideas for Understanding the World');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Concurrent, parallel and distributed computing',
        'Artificial Intelligence: Powerful New Ideas for Understanding the World');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Information and coding theory', 'Artificial Intelligence: Powerful New Ideas for Understanding the World');


# -- book subfield nr. 004 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Artificial intelligence', 'Artificial Intelligence: Powerful New Ideas for Understanding the World');


# ---- data book nr. 023 ---- 
INSERT INTO books (year, rating, pages, publisher, title)
VALUES (2022, 1, 492, 'Del Rey Books', 'Parallel Programming: Accelerating Science, Engineering, and Technology');


# ---- data book authors ---- 

# -- data author nr. 001 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Ledner', 'Lorine');

# -- data book author nr. 001 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Parallel Programming: Accelerating Science, Engineering, and Technology', 'Ledner', 'Lorine');


# -- data author nr. 002 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Wehner', 'Bryan');

# -- data book author nr. 002 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Parallel Programming: Accelerating Science, Engineering, and Technology', 'Wehner', 'Bryan');


# -- data author nr. 003 -- 
INSERT INTO authors (last_name, first_name)
VALUES ('Kerluke', 'Dede');

# -- data book author nr. 003 -- 
INSERT INTO book_authors (book, author_last_name, author_first_name)
VALUES ('Parallel Programming: Accelerating Science, Engineering, and Technology', 'Kerluke', 'Dede');


# ---- data book subfields ---- 

# -- book subfield nr. 001 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Computer networks', 'Parallel Programming: Accelerating Science, Engineering, and Technology');


# -- book subfield nr. 002 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Concurrent, parallel and distributed computing',
        'Parallel Programming: Accelerating Science, Engineering, and Technology');


# -- book subfield nr. 003 -- 
INSERT INTO book_subfields (subfield, book)
VALUES ('Scientific computing applications', 'Parallel Programming: Accelerating Science, Engineering, and Technology');

