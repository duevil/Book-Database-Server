# noinspection SpellCheckingInspectionForFile

DROP TABLE IF EXISTS books;
CREATE TABLE books
(
    id                   SMALLINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    authors              VARCHAR(600),
    editors              VARCHAR(200),
    title                VARCHAR(500) NOT NULL,
    year_of_publication  INTEGER      NOT NULL,
    place_of_publication VARCHAR(50),
    publisher            VARCHAR(200),
    pages                INTEGER      NOT NULL,
    keywords             VARCHAR(1000)
);

INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Analyse zur Entgeltstatistik 2020.', 2022, 'Nürnberg', NULL, 28,
        'Frau; Pandemie; Einkommensunterschied; Inländer; Betriebsgröße; Einkommenseffekt; Erwerbstätigkeit; Lohnentwicklung; Lohnstruktur; Berufsgruppe; Qualifikationsniveau; Auswirkung; Entwicklung; Regionaler Vergleich; Sektorale Verteilung; Ausländer; Mann');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kerst, Christian; Weilage, Insa; Gehrke, Birgit', NULL,
        'Bildung und Qualifikation als Grundlage der technologischen Leistungsfähigkeit Deutschlands 2022.', 2022,
        'Berlin', NULL, 65,
        'Bildungsbeteiligung; Informatik; Mathematik; Naturwissenschaften; Technologische Entwicklung; Partizipation; Beruf; Berufsstruktur; Beschäftigungsstruktur; Qualifikationsstruktur; Betriebliche Weiterbildung; Hochschulbildung; Studium; Studienberechtigter; Studienerfolg; Weiterbildung; Internationaler Vergleich; Auswirkung; Quote; Sektorale Verteilung; Technik; Hochschulabsolvent; OECD (Organisation für wirtschaftliche Zusammenarbeit und Entwicklung); Ausländer; Student; Studentin; Studienanfänger; Europa');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Holger, Arndt; Albrecht, Christian; Conrad, Maren; Preisinger, Alexander; Schwingeler, Stephan; Steinmaurer, Alexander; Gütl, Christian',
        'Arndt, Holger (Hrsg.)', 'Digitale Spiele und fachliches Lernen. Band 1.', 2022, 'Erlangen',
        'FAU University Press', 263,
        'Computerspiel; Fachdidaktik; Unterrichtsmethode; Medienpädagogik; Unterricht; Didaktik');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Maier, Thomas', NULL, 'HTML & CSS.', 2022, 'Graz', 'css4', 186,
        'Lernprozess; Allgemeine Didaktik; Übung; Unterrichtsplanung; Arbeitsblatt; Lernplattform; Open Educational Resources; Unterrichtsmaterial; Elektronische Datenverarbeitung; HTML; Informatik; Informatikunterricht; Web-Design; Blended Learning');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Maier, Thomas', NULL, 'JavaScript. Teacher edition.', 2022, 'Graz', 'css4.at', 197,
        'Lernprozess; Allgemeine Didaktik; Übung; Unterrichtsplanung; Arbeitsblatt; Lernplattform; Open Educational Resources; Unterrichtsmaterial; Informatik; Informatikunterricht; Programmiersprache; Blended Learning; JAVA (Programmiersprache)');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL,
        'Becher, Andrea (Hrsg.); Blumberg, Eva (Hrsg.); Goll, Thomas (Hrsg.); Michalik, Kerstin (Hrsg.); Tenberge, Claudia (Hrsg.)',
        'Sachunterricht in der Informationsgesellschaft.', 2022, 'Bad Heilbrunn', 'Verlag Julius Klinkhardt', 159,
        'Empirische Forschung; Explorative Studie; Denken; Einstellung (Psy); Selbsteinschätzung; Selbstkonzept; Test; Digitale Medien; Informationsgesellschaft; Mediendidaktik; Medieneinsatz; Medienkompetenz; Medienpädagogik; Elementarbereich; Primarbereich; Lehrer; Lehramtsstudent; Lehrerausbildung; Lehrerbildung; Historisches Lernen; Lehr-Lern-Prozess; Didaktische Rekonstruktion; Lernort; Unterrichtsgestaltung; Lehr-Lern-System; Lernplattform; Video; Außerschulischer Lernort; Fachdidaktik; Projekt; Geografie; Gedenkstätte; Geschichte (Histor); Zeitzeuge; Digitalisierung; Digitaltechnik; Informatik; Informatikunterricht; Virtuelle Realität; Rasch Analysis; Naturwissenschaftlicher Unterricht; Reflexion (Phil); Ungleichheit; Bildung für nachhaltige Entwicklung; Flucht; Politische Bildung; Sachunterricht; Hochschullehre; Aufgabenstellung; Lebenswelt; Jugendlicher');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Geis-Thöne, Wido', NULL, 'Zuwanderung aus Indien: Ein großer Erfolg für Deutschland.', 2022, 'Köln', NULL, 35,
        'Informatik; Mathematik; Naturwissenschaften; Altersstruktur; Bevölkerungsstruktur; Einwanderung; Migration; Beschäftigungsentwicklung; Beruf; Nachwuchs; Qualifikationsstruktur; Fachkraft; Auswanderung; Auswirkung; Entwicklung; Quote; Regionaler Vergleich; Rekrutierung; Technik; Ziel; Ausländer; Einwanderer; Hoch Qualifizierter; Herkunftsland; Indien');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Teichert, Christian', NULL, 'Arbeitsmarkteinstieg und räumliche Mobilität von Hochschulabsolvent*innen.', 2021,
        'Hannover', NULL, 136,
        'Geographie; Mobilität; Dissertation; Verweildauer; Analyse; Hochschulabsolvent; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Gehrke, Birgit; Kerst, Christian; Wieck, Markus; Weilage, Insa', NULL,
        'Bildung und Qualifikation als Grundlage der technologischen Leistungsfähigkeit Deutschlands 2021.', 2021,
        'Berlin', 'Expertenkommission Forschung und Innovation (EFI)', 156,
        'Bildungsbeteiligung; Forschungsförderung; Bildungsertrag; Informatik; Mathematik; Naturwissenschaften; Technologische Entwicklung; Beteiligung; Wirtschaftssystem; Beruf; Berufsstruktur; Erwerbstätiger; Qualifikationsstruktur; Ausbildungsplatzangebot; Betriebliche Berufsausbildung; Betriebliche Weiterbildung; Ausbildungsquote; Hochschulbildung; Studium; Studienerfolg; Weiterbildung; Dritter Bildungsweg; Internationaler Vergleich; Abbruch; Auswirkung; Sektorale Verteilung; Struktur; Technik; Hochschulabsolvent; OECD (Organisation für wirtschaftliche Zusammenarbeit und Entwicklung); Ausländer; Student; Wissenschaftler; Ausland; Europa');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Freigang, Sirkka', NULL, 'Das Internet der Dinge für Bildung nutzbar machen.', 2021, 'Wiesbaden',
        'Springer VS', 351,
        'Bildungsforschung; Künstliche Intelligenz; Mediennutzung; Lehr-Lern-Prozess; Lernen; Lernumgebung; Arbeitswelt; Informatik; Lebenslanges Lernen; Interdisziplinarität; Internet');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'MINT Nachwuchsbarometer 2021.', 2021, 'München', NULL, 28,
        'Informatik; Mathematik; Naturwissenschaften; Technik; Wissenschaftlicher Nachwuchs');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Neugebauer, Martin; Bröder, Charlotte; Daniel, Annabell', NULL, 'Studienausstieg - und dann?', 2021,
        'Bielefeld', 'wbv', 45,
        'Befragung; Erfahrung; Stichprobe; Übergang; Informatik; Regressionsanalyse; Unternehmen; Ausbildung; Arbeitsmarkt; Berufstätigkeit; Beschäftigung; Bewerbung; Berufsfeld; Studium; Wirtschaftswissenschaft; Abbruch; Bewertung; Arbeitgeber; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Neumann, Irene; Rohenroth, Dunja; Heinze, Aiso', NULL, 'Studieren ohne Mathe?', 2021, 'Kiel',
        'IPN - Leibniz-Institut für die Pädagogik der Naturwissenschaften und Mathematik', 56,
        'Lernen; Mathematik; Studium; Voraussetzung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Landwehr, Brunhild (Hrsg.); Mammes, Ingelore (Hrsg.); Murmann, Lydia (Hrsg.)',
        'Technische Bildung im Sachunterricht der Grundschule. Elementar bildungsbedeutsam und dennoch vernachlässigt?',
        2021, 'Bad Heilbrunn', 'Verlag Julius Klinkhardt', 169,
        'Informationstechnologische Bildung; Bildungsprozess; Empirische Untersuchung; Fragebogenerhebung; Attribuierungstheorie; Interventionsstudie; Präkonzept; Selbstkonzept; Vorstellung (Psy); Medienausstattung; Medienerziehung; Technische Medien; Schuljahr 03; Schuljahr 04; Schuljahr 05; Grundschule; Primarbereich; Schüler; Lernen; Computer; Fachdidaktik; Informatik; Erneuerbare Energie; Technikunterricht; Technisches Denken; Sachunterricht; Effekt; Technik; Technisches Wissen; Deutschland; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schauer, Carola; Schwarz, Tobias', NULL,
        'Wie sollte man Studieninteressierte über Wirtschaftsinformatik informieren?', 2021, 'Essen',
        'Institut für Informatik und Wirtschaftsinformatik (ICB), Universität Duisburg-Essen', 85,
        'Weiterführende Schule; Lehrplan; Wirtschaftsinformatik; Berufswahl; Deutschland; Nordrhein-Westfalen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schauer, Carola', NULL,
        'Wirtschaftsinformatik-Studiengänge an Universitäten in Deutschland: Analyse der Studienanfängerzahlen und Frauenanteile im Vergleich zur Informatik und zu Fachhochschulen.',
        2021, 'Essen', 'Institut für Informatik und Wirtschaftsinformatik (ICB), Universität Duisburg-Essen', 75,
        'Frau; Wirtschaftsinformatik; Studienfach; Student; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Gehrke, Birgit; Kerst, Christian; Weilage, Insa', NULL,
        'Bildung und Qualifikation als Grundlage der technologischen Leistungsfähigkeit Deutschlands 2020 (Kurzstudie).',
        2020, 'Berlin', 'Expertenkommission Forschung und Innovation (EFI)', 50,
        'Kompetenz; Bildungsbeteiligung; Bildungsverlauf; Bildungssystem; Durchlässigkeit; Arbeitswelt; Digitalisierung; Informatik; Mathematik; Naturwissenschaften; Partizipation; Wettbewerbsfähigkeit; Ausbildung; Arbeitskräfteangebot; Arbeitskräftebedarf; Beruf; Berufsbildungssystem; Berufsverlauf; Erwerbstätiger; Qualifikationsstruktur; Betriebliche Berufsausbildung; Betriebliche Weiterbildung; Hochschulbildung; Bologna-Prozess; Hochschulreform; Studienberechtigter; Dualer Studiengang; Weiterbildung; Dritter Bildungsweg; Internationaler Vergleich; Technik; Hochschulabsolvent; OECD (Organisation für wirtschaftliche Zusammenarbeit und Entwicklung); Absolvent; Ausländer; Student; Studienanfänger; Europa');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL,
        'Kaspar, Kai (Hrsg.); Becker-Mrotzek, Michael (Hrsg.); Hofhues, Sandra (Hrsg.); König, Johannes (Hrsg.); Schmeinck, Daniela (Hrsg.)',
        'Bildung, Schule, Digitalisierung.', 2020, 'Münster; New York', 'Waxmann', 478,
        'Kompetenz; Zukunftsperspektive; Empirische Forschung; Interdisziplinäre Forschung; Kompetenzerwerb; Bildungsbiografie; Digitale Medien; Digitale Revolution; Medien; Mediendidaktik; Medieneinsatz; Medienerziehung; Medienforschung; Mediennutzung; Medienpädagogik; Schule; Lehrerausbildung; Lehrerbildung; Lehreraustausch; Lehrerkooperation; Lehrer-Alltag; Lehrer-Lehrer-Beziehung; Lernbedingungen; Lernen; Lernprozess; Lernmethode; Computerprogramm; Computerunterstützter Unterricht; Software; Fächerübergreifender Unterricht; Interdisziplinärer Unterricht; Digitalisierung; Informatik; Transformation; Berufsschule; Kompetenzentwicklung; Zukunftserwartung; Zukunftsplanung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Maier, Tobias; Zika, Gerd; Kalinowski, Michael; Steeg, Stefanie; Mönning, Anke; Wolter, Marc Ingo; Hummel, Markus; Schneemann, Christian',
        NULL, 'COVID-19-Krise: Die Arbeit geht weiter, der Wohlstand macht Pause.', 2020, NULL, NULL, 20,
        'Pandemie; Wirtschaftsentwicklung; Beschäftigungsentwicklung; Arbeitsmarktentwicklung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Zender, Raphael (Hrsg.); Ifenthaler, Dirk (Hrsg.); Leonhardt, Thiemo (Hrsg.); Schumacher, Clara (Hrsg.)',
        'DELFI 2020 - die 18. Fachtagung Bildungstechnologien der Gesellschaft für Informatik e. V. 14.-18. September 2020. Online.',
        2020, 'Bonn', 'Gesellschaft für Informatik', 390, 'Medienpädagogik; Bildungstechnologie');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL,
        'Vollmer, Thomas (Hrsg.); Karges, Torben (Hrsg.); Richter, Tim (Hrsg.); Schlömer, Britta (Hrsg.); Schütt-Sayed, Sören (Hrsg.)',
        'Digitalisierung mit Arbeit und Berufsbildung nachhaltig gestalten.', 2020, 'Bielefeld', 'wbv', 217,
        'Neue Medien; Schüler; Lernerfolg; Lernen; Unterricht; Digitalisierung; Berufsausbildung; Berufsbildung; Berufliche Qualifikation; Berufspraxis; Karriere; Berufsorientierung; Berufsschule; Aufsatzsammlung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Aufenanger, Stefan; Daum, Timo; Diethelm, Ira', NULL, 'Gesellschaft digital?', 2020, 'Frankfurt am Main',
        'Gewerkschaft Erziehung und Wissenschaft', 23,
        'Bildung; Gesellschaft; Medienkompetenz; Medienpädagogik; Unterricht; Digitalisierung; Informatik; Gewerkschaftsarbeit; Kapitalismus; Gewerkschaft Erziehung und Wissenschaft');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Demary, Vera; Engels, Barbara; Goecke, Henry; Koppel, Oliver; Mertens, Armin; Rusche, Christian; Scheufen, Marc; Wendt, Jan',
        NULL, 'KI-Monitor 2020.', 2020, 'Köln', NULL, 65,
        'Künstliche Intelligenz; Massenmedien; Soziale Software; Indikator; Informatik; Technologiepolitik; Öffentliche Meinung; Patent; Public Private Partnership; Unternehmen; Forschungsstand; Anwendung; Entwicklung; Forschungsumsetzung; Infrastruktur; Regulation; Veröffentlichung; Internet; Hochschulabsolvent');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Medien und Informatik.', 2020, 'Luzern', 'Dienststelle Volksschulbildung', 23,
        'Medien; Unterricht; Informatik; Weiterbildung; Ausstattung; Luzern');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Köller, Olaf; Steffensky, Mirjam', NULL, 'MINT Nachwuchsbarometer 2020.', 2020, 'München', NULL, 22,
        'Bildungsmonitoring; Bildungssystem; Informatik; Mathematik; Naturwissenschaften; Nachwuchs; Nachwuchsförderung; Technik; Trendbericht; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'MINT-Aktionsplan und zielgruppengerichtete Ansprachen von Mädchen und Frauen.', 2020, 'Berlin',
        'Bundestag', 7,
        'Bildung; Informationstechnologische Bildung; Frau; Bildungsförderung; Region; Informatik; Mathematik; Cluster; Naturwissenschaften; Naturwissenschaftliche Bildung; Beruf; Berufsorientierung; Aktionsplan; Technik; Mädchen; Weibliche Jugendliche');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Kohlisch, Enno; Koppel, Oliver; Plünnecke, Axel; Schüler, Ruth Maria', NULL,
        'MINT-Frühjahrsreport 2020.', 2020, 'Köln', NULL, 98,
        'Bildungsexpansion; Frau; Informatik; Mathematik; Naturwissenschaften; Erwerbsstatistik; Erwerbstätigkeit; Arbeitskräftebedarf; Arbeitslosigkeit; Arbeitsplatzangebot; Beruf; Erwerbstätiger; Fachkraft; Innovation; Regionalverteilung; Technik; Älterer Arbeitnehmer; Migrant; Sozialversicherungspflichtiger Arbeitnehmer');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Kohlisch, Enno; Koppel, Oliver; Plünnecke, Axel', NULL, 'MINT-Herbstreport 2020.', 2020,
        'Köln', NULL, 137,
        'Pandemie; Informatik; Mathematik; Naturwissenschaften; Arbeitsbedingungen; Beschäftigungseffekt; Elektrotechnische Industrie; Lohnentwicklung; Beschäftigungsentwicklung; Arbeitskräfteangebot; Arbeitskräftebedarf; Arbeitskräftemangel; Arbeitslosigkeit; Arbeitsmarktentwicklung; Beruf; Nachwuchs; Fachkraft; Metallindustrie; Auswirkung; Technik');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Jeanrenaud, Yves', NULL, 'MINT. Warum nicht?', 2020, 'Berlin', NULL, 58,
        'Kultur; Stereotyp; Frau; Frauenförderung; Erfolgskontrolle; Informatik; Mathematik; Naturwissenschaften; Gleichstellungspolitik; Beruf; Berufsorientierung; Berufswahl; Informationstechnischer Beruf; Studienwahl; Geschlechterverteilung; Geschlechtsspezifik; Quote; Technik');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Maier, Thomas', NULL, 'PHP und MySQL.', 2020, 'Graz', 'css4', 240,
        'Hypertext; Lernprozess; Allgemeine Didaktik; Übung; Unterrichtsplanung; Arbeitsblatt; Lernplattform; Open Educational Resources; Unterrichtsmaterial; Datenbanksystem; Informatik; Informatikunterricht; Programmiersprache; Blended Learning; SQL');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Boudreau, Kevin; Kaushik, Nilam', NULL,
        'The Gender Gap in Tech & Competitive Work Environments? Field Experimental Evidence from an Internet-of-Things Product Development Platform.',
        2020, NULL, NULL, 54,
        'Frau; Frauenberuf; Informatik; Mathematik; Naturwissenschaften; Spitzentechnologie; Produktgestaltung; Unternehmenskultur; Wettbewerbsfähigkeit; Beruf; Teamkompetenz; Männerberuf; Informationswirtschaft; Ökonomie; Geschlechtsspezifik; Kooperationsbereitschaft; Technik; Internet; Hochschulabsolvent; Mann; USA');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Bartel, Paula', NULL, 'Aufgabenorientierte Hochschullehre.', 2019, 'Augsburg', NULL, 343,
        'Aufgabenorientierung; Lernaufgabe; Hochschuldidaktik; Hochschullehre; Dissertation; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Damberger, Thomas', NULL, 'Bildung im Digitalzeitalter.', 2019, 'Magdeburg',
        'Universitätsbibliothek Otto-von-Guericke-Universität', 408,
        'Bildung; Pädagogik; Medienpädagogik; Digitalisierung; Digitaltechnik; Habilitationsschrift');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Gehrke, Birgit; Kerst, Christian; Wieck, Markus; Trommer, Max; Weilage, Insa', NULL,
        'Bildung und Qualifikation als Grundlage der technologischen Leistungsfähigkeit Deutschlands 2019.', 2019,
        'Berlin', NULL, 145,
        'Kompetenz; Bildungsbeteiligung; Bildungsverlauf; Bildungssystem; Durchlässigkeit; Arbeitswelt; Digitalisierung; Informatik; Mathematik; Naturwissenschaften; Partizipation; Erwerbstätigkeit; Wettbewerbsfähigkeit; Ausbildung; Arbeitskräfteangebot; Arbeitskräftebedarf; Arbeitsmarktentwicklung; Berufsbildungssystem; Berufsverlauf; Beschäftigungsstruktur; Erwerbstätiger; Qualifikationsstruktur; Naturwissenschaftlich-technischer Beruf; Betriebliche Berufsausbildung; Betriebliche Weiterbildung; Hochschulbildung; Bologna-Prozess; Hochschulreform; Studienberechtigter; Dualer Studiengang; Auslandsstudium; Weiterbildung; Dritter Bildungsweg; Internationaler Vergleich; Prognose; Technik; Hochschulabsolvent; OECD (Organisation für wirtschaftliche Zusammenarbeit und Entwicklung); Absolvent; Ausländer; Student; Studienanfänger; Europa');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Hoff, Oliver; Brandes, Julia', NULL, 'Einführung des Obligatorischen Fachs Informatik (OFI).', 2019,
        '[Zürich]', 'SVIA', 49,
        'Kompetenz; Wissen; Gymnasium; Maturitätsschule; Lehrer; Fachlehrer; Lernen; Digitalisierung; Informatik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL,
        'Jörissen, Benjamin (Hrsg.); Kröner, Stephan (Hrsg.); Unterberg, Lisa (Hrsg.); Schmiedl, Friederike (Mitarb.)',
        'Forschung zur Digitalisierung in der Kulturellen Bildung.', 2019, 'München', 'kopaed', 236,
        'Bildung; Pädagogik; Kultur; Erziehungswissenschaft; Kreativität; Bildungsangebot; Forschungsförderung; Projektunterricht; Software; Literatur; Digitalisierung; Informatik; Bildende Kunst; Musische Erziehung; Musik; Musikpädagogik; Technologie; Tanz; Sportwissenschaft; Erwachsenenbildung; Behindertenpädagogik; Gestaltung; Interdisziplinarität; Jugendlicher; Forschungsprojekt');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Frauen für die duale MINT-Ausbildung gewinnen.', 2019, 'Bonn', 'Bundesinstitut für Berufsbildung',
        13,
        'Frau; Informatik; Mathematik; Naturwissenschaften; Berufsausbildung; Beruf; Best-Practice-Modell; Technik');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Friedrich, Julius-David; Hachmeister, Cort-Denis; Nickel, Sigrun; Peksen, Sude; Roessler, Isabel; Ulrich, Saskia',
        NULL, 'Frauen in IT: Handlungsempfehlungen zur Gewinnung von Frauen für Informatik.', 2019, 'Gütersloh', NULL,
        17,
        'Frau; Informatik; Berufsberatung; Berufsinformation; Informationstechnischer Beruf; Studium; Studienwahl; Studienberatung; Geschlechterverteilung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Hochschulpakt 2020. Bericht zur Umsetzung im Jahr 2017.', 2019, 'Bonn',
        'Gemeinsame Wissenschaftskonferenz (GWK)', 125,
        'Bildung; Hochschulgeschichte; Frau; Zeitschrift; Förderung; Bundesland; Staat; Ausgaben; Finanzierung; Personalentwicklung; Fachhochschule; Hochschulbildung; Hochschulpolitik; Studentenzahl; Professur; Studienerfolg; Statistik; Studienfach; Hochschule; Bericht; Qualitätssicherung; Umsetzung; Studienanfänger; Wissenschaftlicher Nachwuchs; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Maier, Thomas', NULL, 'HTML und CSS.', 2019, 'Graz', 'css4', 175,
        'Lernprozess; Allgemeine Didaktik; Übung; Unterrichtsplanung; Arbeitsblatt; Lernplattform; Open Educational Resources; Unterrichtsmaterial; Elektronische Datenverarbeitung; HTML; Informatik; Informatikunterricht; Web-Design; Blended Learning');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Frohn, Julia (Hrsg.); Brodesser, Ellen (Hrsg.); Moser, Vera (Hrsg.); Pech, Detlef (Hrsg.)',
        'Inklusives Lehren und Lernen. Allgemein- und fachdidaktische Grundlagen.', 2019, 'Bad Heilbrunn',
        'Verlag Julius Klinkhardt', 209,
        'Integrative Pädagogik; Pädagogische Praxis; Empirische Untersuchung; Interdisziplinäre Forschung; Einstellung (Psy); Selbstwirksamkeit; Lehrbefähigung; Lehramtsstudent; Lehrerbildung; Heterogene Klasse; Adaptiver Unterricht; Didaktisches Modell; Integrative Didaktik; Heterogene Lerngruppe; Fachdidaktik; Inklusion; Sprachbildung; Lateinunterricht; Englischunterricht; Geschichtsunterricht; Informatikunterricht; Sachunterricht; Professionalisierung; Integrative Behindertenpädagogik; Sonderpädagogik; Heterogenität; Integration; Perspektive; Forschungsprojekt; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Maier, Thomas', NULL, 'JavaScript.', 2019, 'Graz', 'css4', 187,
        'Lernprozess; Allgemeine Didaktik; Übung; Unterrichtsplanung; Arbeitsblatt; Lernplattform; Open Educational Resources; Unterrichtsmaterial; Informatik; Informatikunterricht; Programmiersprache; Blended Learning; JAVA (Programmiersprache)');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Wittpahl, Volker (Mitarb.)', 'Künstliche Intelligenz.', 2019, 'Berlin; Heidelberg', 'Springer Vieweg',
        286,
        'Künstliche Intelligenz; Arbeitswelt; Informatik; Berufsbildung; Beruf; Berufspädagogik; Wissenschaft; Erwachsenenbildung; Technik; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Brück-Klingberg, Andrea; Althoff, Jörg', NULL,
        'MINT-Berufe: Strukturen und Trends der Beschäftigung in Bremen.', 2019, 'Nürnberg', NULL, 45,
        'Informatik; Mathematik; Naturwissenschaften; Beschäftigungsentwicklung; Berufsausbildung; Arbeitslosigkeit; Ausbildungsquote; Studium; Geschlechterverteilung; Technik; Bremen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Mit MINT in die Zukunft!', 2019, 'Berlin', 'Bundesministerium für Bildung und Forschung (BMBF)',
        23,
        'Gesellschaft; Transfer; Mädchenförderung; Bildungspolitik; Frauenförderung; Förderung; Förderungsmaßnahme; Kind; Informatik; Mathematik; Naturwissenschaften; Berufliche Bildung; Fachkraft; Aktionsplan; Attraktivität; Handlungsfeld; Technik; Bundesministerium für Bildung und Forschung; Jugendlicher; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Jost, Oskar; Seibert, Holger; Wiethölter, Doris', NULL,
        'Regionale Mobilität von Lehrlingen: Auszubildende in MINT-Berufen pendeln besonders häufig.', 2019, 'Nürnberg',
        NULL, 8,
        'Schulabschluss; Regionale Mobilität; Informatik; Mathematik; Naturwissenschaften; Beruf; Ausbildungsplatzangebot; Ausbildungsberuf; Betriebliche Berufsausbildung; Technik; Auszubildender; Jugendlicher; Pendler');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Amann, Flavia; u.a.', NULL,
        'Schlussevaluation des Praxisintegrierten Bachelorstudiengangs PiBS an Fachhochschulen.', 2019, 'Zürich',
        'econcept', 146,
        'Modellversuch; Informatik; Mathematik; Naturwissenschaften; Fachhochschule; Forschungsbericht; Technik; Zulassungsbedingung; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Fehrmann, Raphael', NULL,
        'Stop-Motion-Videos in inklusiven Settings des Mathematikunterrichts der Grundschule.', 2019, 'Münster',
        'Universitäts- und Landesbibliothek Münster', 22,
        'Digitale Medien; Mediendidaktik; Medienkompetenz; Medienpädagogik; Grundschule; Primarbereich; Fächerübergreifendes Lernen; Computerunterstützter Unterricht; Lehrfilm; Unterrichtsmedien; Video; Inklusion; Fächerübergreifender Unterricht; Informatik; Mathematik; Mathematikunterricht; Rechenunterricht; Sonderpädagogik; Erklären');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Grillenberger, Andreas', NULL,
        'Von Datenmanagement zu Data Literacy: Informatikdidaktische Aufarbeitung des Gegenstandsbereichs Daten für den allgemeinbildenden Schulunterricht.',
        2019, 'Berlin', 'Freie Universität Berlin', 330,
        'Kompetenz; Didaktische Rekonstruktion; Unterricht; Fachdidaktik; Datenerfassung; Datenmanagement; Datenspeicherung; Informatikunterricht; Daten; Nutzung; Datenverarbeitung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Jann, Ben; Hupka-Brunner, Sandra', NULL,
        'Warum werden Frauen so selten MINT-Fachkräfte? Zur Bedeutung der Differenz zwischen mathematischen Kompetenzen und Selbstkonzept.',
        2019, 'Bern', 'Transition from Education to Employment (TREE)', 24,
        'Kompetenz; Geschlechterrolle; Geschlechtsunterschied; Übergang Sekundarstufe I - Sekundarstufe II; Informatik; Mathematik; Naturwissenschaften; Fachkraft; Forschungsbericht; Mangel; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Ebert, Michael', NULL,
        'Webbasierte Ad-hoc-Programmieraufgaben zur Vermittlung von grundlegenden Konzepten der Programmierung in Vorlesungen.',
        2019, 'Bayreuth', 'Universität Bayreuth', 289, 'Lehren; Programmierung; Vorlesung; Dissertation; Aufgabe');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Ausbildung von Lehrerinnen und Lehrern für Informatik am Gymnasium.', 2018, 'Bern', 'EDK', 13,
        'Lehrerbildung; Maturität; Informatik; Konzept; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Jenkner, Peter; Deuse, Carsten; Dölle, Frank; Funke, Johanna; Sanders, Sandra; Winkelmann, Gert', NULL,
        'Ausstattungs-, Kosten- und Leistungsvergleich Universitäten 2016.', 2018, 'Hannover', NULL, 95,
        'Staat; Statistik; Hochschule; Berlin; Bremen; Hamburg; Mecklenburg-Vorpommern; Sachsen-Anhalt; Schleswig-Holstein');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Morlang, Katharina', 'Becker, Jörg (Mitarb.); Druba, Lisa (Mitarb.)', 'Bildung - Jugend - Sport.', 2018,
        'Frankfurt a.M.', 'Deutsche Sportjugend', 26,
        'Bildung; Bildungspolitik; Projekt; Informatik; Nachhaltigkeit; Breitensport; Kinder- und Jugendsport; Sport; Sportorganisation; Sportpolitik; Sportsoziologie; Gemeinnützigkeit; Gestaltung; Strategie; Sportjugend; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Gehrke, Birgit; Kerst, Christian', NULL,
        'Bildung und Qualifikation als Grundlage der technologischen Leistungsfähigkeit Deutschlands 2018 (Kurzstudie).',
        2018, 'Berlin', NULL, 48,
        'Kompetenz; Bildungsbeteiligung; Bildungsverlauf; Bildungssystem; Durchlässigkeit; Informatik; Mathematik; Naturwissenschaften; Teilnahme; Wettbewerbsfähigkeit; Ausbildung; Berufsausbildung; Arbeitskräfteangebot; Arbeitskräftebedarf; Arbeitsmarktpolitik; Beruf; Berufsbildungssystem; Berufsverlauf; Erwerbstätiger; Qualifikationsstruktur; Hochschulbildung; Bologna-Prozess; Hochschulreform; Studienberechtigter; Dualer Studiengang; Weiterbildung; Dritter Bildungsweg; Internationaler Vergleich; Technik; Hochschulabsolvent; OECD (Organisation für wirtschaftliche Zusammenarbeit und Entwicklung); Absolvent; Ausländer; Studienanfänger; Europa');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Krömker, Detlef (Hrsg.); Schroeder, Ulrik (Hrsg.)', 'DeLFI 2018.', 2018, 'Bonn',
        'Gesellschaft für Informatik', 310,
        'Bildung; Autismus; Digitale Medien; Medieneinsatz; Medienkompetenz; Mediennutzung; Nutzerverhalten; Schule; Lehrerfortbildung; Leistungsbeurteilung; Lernprozess; Lernumgebung; Computerspiel; Lernplattform; Soziale Software; Videoaufzeichnung; Selbstgesteuertes Lernen; Algorithmus; Digitalisierung; Mensch-Maschine-Kommunikation; Virtuelle Realität; Automatisierung; Naturkatastrophe; Hochschullehrer; Wissenschaftliches Arbeiten; Blended Learning; E-Learning; Analyse; Aufgabenstellung; Bewertung; Konferenzschrift; Tagungsbericht; Logdatei; World Wide Web');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kay, Rosemarie; Nielen, Sebastian', NULL,
        'Die Beschäftigungs- und Einkommenssituation von Young Women MINT Professionals im Mittelstand.', 2018, 'Bonn',
        NULL, 43,
        'Mittelstand; Frau; Informatik; Determinante; Mathematik; Naturwissenschaften; Einkommensunterschied; Gleichstellungspolitik; Lohnhöhe; Personalauswahl; Beschäftigungsentwicklung; Arbeitsmarktchance; Beruf; Berufschance; Berufsstruktur; Beschäftigtenstatistik; Geschlechterverteilung; IAB-Linked-Employer-Employee-Datensatz; Sektorale Verteilung; Technik; Hochschulabsolvent; Berufsanfänger; Führungskraft; Junger Erwachsener; Mann');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Friedrich, Julius-David; Hachmeister, Cort-Denis; Nickel, Sigrun; Peksen, Sude; Roessler, Isabel; Ulrich, Saskia',
        NULL, 'Frauen in Informatik.', 2018, 'Dortmund', NULL, 80,
        'Baden-Württemberg; Deutschland; Mecklenburg-Vorpommern; Thüringen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Bergner, Nadine; Hubwieser, Peter; Köster, Hilde; Magenheim, Johannes; Müller, Kathrin; Romeike, Ralf; Schroeder, Ulrik; Schulte, Carsten',
        NULL, 'Frühe informatische Bildung - Ziele und Gelingensbedingungen für den Elementar- und Primarbereich.',
        2018, 'Opladen; Berlin', 'Verlag Barbara Budrich', 351,
        'Kompetenz; Elementarbildung; Kompetenzerwerb; Digitale Medien; Kind; Frühpädagogik; Grundschule; Fachkompetenz; Informatik; Pädagogische Fachkraft; Empfehlung; Expertise; Deutschland; Großbritannien; Neuseeland; Schweiz; Vereinigte Staaten');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Hochschulpakt 2020. Bericht zur Umsetzung im Jahr 2016.', 2018, 'Bonn',
        'Gemeinsame Wissenschaftskonferenz (GWK)', 115,
        'Bildung; Frau; Geschichte (Histor); Bundesland; Staat; Ausgaben; Finanzierung; Personalentwicklung; Fachhochschule; Hochschulbildung; Studentenzahl; Professur; Statistik; Hochschule; Bericht; Qualitätsentwicklung; Mitarbeiter; Studienanfänger; Wissenschaftlicher Nachwuchs; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schüpbach, Sabrina; Michel, Iris; Schönbächler, Marie-Theres', NULL,
        'Innovative Bildungsprojekte und -programme als Antwort auf den digitalen Wandel.', 2018, 'Bern', 'PH Bern', 6,
        'Audiovisuelles Medium; Interaktives Video; Multimediale Methode; Sprachunterricht; Projekt; Digitalisierung; Informatik; Mathematik; Naturwissenschaften; Weiterbildung; Technik; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Plünnecke, Axel; Schüler, Ruth Maria', NULL, 'INSM-Bildungsmonitor 2018.', 2018, 'Köln',
        NULL, 245,
        'Bildung; Informationstechnologische Bildung; Bildungschance; Bildungsdefizit; Chancengleichheit; Bildungssystem; Bildungspolitik; Benachteiligtenförderung; Bildungsförderung; Forschungsförderung; Schulklasse; Klassengröße; Schulleistung; Computerprogramm; Computerkenntnisse; Informatik; Mathematik; Naturwissenschaften; Technologische Entwicklung; Armut; Bundesland; Internationalisierung; Ausgaben; Berufsbildung; Beruf; Personalbestand; Hochschulzugang; Hochschule; Auswirkung; Regionaler Vergleich; Technik; Bildungseinrichtung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'IT-Fachleute.', 2018, 'Nürnberg', NULL, 18,
        'Elektronische Datenverarbeitung; Informatik; Beschäftigungsentwicklung; Arbeitskräftebedarf; Arbeitskräftemangel; Arbeitslosenquote; Arbeitsplatzangebot; Stellenbesetzung; Fachkraft; Informationstechnischer Beruf; Programmierer; Quote; Hochschulabsolvent; Berater; Studienanfänger');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Konzept Medien und Informatik der Volksschule Appenzell Ausserrhoden.', 2018, 'Herisau',
        'Abteilung Volksschule', 17, 'Curriculum; Digitalisierung; Hardware; Konzept; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Winde, Mathias; Schröder, Eike', NULL, 'Ländercheck Informatik.', 2018, 'Essen', NULL, 48,
        'Prüfungsordnung; Informatik; Föderalismus; Wissenschaft');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Mehr Schnupperlehrstellen in den MINT-Berufen.', 2018, 'Bern', 'Bundesrat', 14,
        'Informatik; Mathematik; Naturwissenschaften; Berufswahl; Schnupperlehre; Auslese; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Burkert, Carola; Kislat, Julia; Schaade, Peter', NULL,
        'MINT-Berufe: Strukturen und Trends der Beschäftigung in Hessen.', 2018, 'Nürnberg', NULL, 48,
        'Informatik; Mathematik; Naturwissenschaften; Beschäftigungsentwicklung; Beruf; Regionaler Arbeitsmarkt; Betriebliche Berufsausbildung; Studienerfolg; Geschlechterverteilung; Technik; Studienanfänger; Hessen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Brück-Klingberg, Andrea; Althoff, Jörg', NULL,
        'MINT-Berufe: Strukturen und Trends der Beschäftigung in Niedersachsen.', 2018, 'Nürnberg', NULL, 50,
        'Frau; Frauenberuf; Informatik; Mathematik; Naturwissenschaften; Segregation; Beruf; Qualifikationsanforderung; Männerberuf; Betriebliche Berufsausbildung; Universität; Technische Hochschule; Geschlechterverteilung; Technik; Bremen; Niedersachsen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Koppel, Oliver; Plünnecke, Axel', NULL, 'MINT-Frühjahrsreport 2018.', 2018, 'Köln', NULL, 98,
        'Bildungsexpansion; Informatik; Mathematik; Naturwissenschaften; Erwerbsstatistik; Erwerbstätigkeit; Arbeitskräftebedarf; Arbeitslosigkeit; Arbeitsplatzangebot; Beruf; Erwerbstätiger; Fachkraft; Innovation; Regionalverteilung; Technik; Älterer Arbeitnehmer; Migrant; Sozialversicherungspflichtiger Arbeitnehmer');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Koppel, Oliver; Plünnecke, Axel; Röben, Enno; Schüler, Ruth Maria', NULL,
        'MINT-Herbstreport 2018.', 2018, 'Köln', NULL, 109,
        'Bildungsexpansion; Frau; Informatik; Mathematik; Naturwissenschaften; Erwerbsstatistik; Erwerbstätigkeit; Arbeitslosigkeit; Arbeitsplatzangebot; Beruf; Erwerbstätiger; Fachkraft; Bedarf; Innovation; Regionalverteilung; Technik; Älterer Arbeitnehmer; Migrant; Sozialversicherungspflichtiger Arbeitnehmer');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'MINT-Talent Monitor.', 2018, 'München', NULL, 34,
        'Mobilitätsbereitschaft; Motivation; Regionale Mobilität; Informatik; Mathematik; Naturwissenschaften; Migration; Einkommenserwartung; Arbeitsmotivation; Arbeitsplatzwahl; Auslandstätigkeit; Berufserwartung; Normalarbeitsverhältnis; Naturwissenschaftlich-technischer Beruf; Unternehmer; Technik; Student');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Schlussbericht Mandat MINT 2013-2016.', 2018, 'Bern', 'Akademien der Wissenschaften Schweiz', 21,
        'Projekt; Informatik; Mathematik; Naturwissenschaften; Ausbildungsangebot; Wissenschaft; Bericht; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Simply Nano 2.', 2018, 'Zürich', 'SimplyScience', 35,
        'Beziehung; Förderung; Schule; Lehrerfortbildung; Informatik; Mathematik; Naturwissenschaften; Unternehmen; Technik; Aargau');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Woisch, Andreas; Renneberg, Ann-Christin; Mentges, Hanna', NULL, 'Wer nimmt ein duales Studium auf?', 2018,
        'Hannover', NULL, 8,
        'Soziale Herkunft; Schulleistung; Migrationshintergrund; Ökonomische Determinanten; Arbeitsplatzsicherheit; Berufschance; Ausbildungswahl; Studienberechtigter; Dualer Studiengang; Studienmotivation; Geschlechterverteilung; Praxisbezug; Student');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Mostovova, Elena; Hetze, Pascal',
        'Tiefenbacher, Alexander (Mitarb.); Friese, Carolin (Mitarb.); Herting, Cornelia (Red.)',
        'Wie international ist MINT?', 2018, 'Essen', 'Edition Stifterverband', 48,
        'Interkulturelles Lernen; Curriculum; Informatik; Mathematik; Ingenieurwissenschaft; Naturwissenschaften; Auslandsbeziehungen; Internationale Zusammenarbeit; Internationaler Wettbewerb; Marketing; Wettbewerbsfähigkeit; Beruf; Hochschulpolitik; Hochschullehrer; Auslandsstudium; Best-Practice-Modell; Technik; Student; Wissenschaftler; Ausland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Roessler, Isabel; Peksen, Sude', NULL,
        'Wie wichtig sind Informatikstudentinnen Praxis- und Arbeitsmarktbezug im Studium?', 2018, 'Gütersloh', NULL,
        44, 'Informatik; Studium; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Roessler, Isabel; Peksen, Sude', NULL,
        'Wie wichtig ist Informatikstudentinnen die inhaltliche Gestaltung des Informatikstudiums?', 2018, 'Gütersloh',
        NULL, 21, 'Informatik; Studium; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Roessler, Isabel; Peksen, Sude', NULL,
        'Wie wichtig sind Informatikstudentinnen digitale Tools und digitale Lehre in der Lehre?', 2018, 'Gütersloh',
        NULL, 22, 'Informatik; Studium; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Peksen, Sude; Roessler, Isabel', NULL,
        'Wie wichtig ist Studentinnen die inhaltliche Gestaltung des Informatikstudiums?', 2018, 'Gütersloh', NULL, 19,
        'Informatik; Studium; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Peksen, Sude; Roessler, Isabel', NULL,
        'Wie wichtig sind Studentinnen digitale Tools und digitale Lehre im Informatikstudium?', 2018, 'Gütersloh',
        NULL, 27, 'Informatik; Studium; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Peksen, Sude; Roessler, Isabel', NULL,
        'Wie wichtig sind Studentinnen Praxis- und Arbeitsmarktbezug im Informatikstudium?', 2018, 'Gütersloh', NULL,
        25,
        'Zufriedenheit; Frau; Curriculum; Informatik; Berufserfahrung; Berufsinteresse; Fachhochschule; Studium; Universität; Praxisbezug; Theorie-Praxis-Beziehung; Student');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Jäckel, Stefanie', NULL, 'Zur Motivierung im Informatikunterricht.', 2018, 'Jena',
        'Friedrich-Schiller-Universität Jena', 30,
        'Schüler; Lernen; Lehren; Informatik; Informatikunterricht; Dissertation; Österreich');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Haberfellner, Regina; Hueber, Brigitte', NULL,
        'Arbeitsmarkt- und Berufstrends im Gesundheitssektor unter besonderer Berücksichtigung des medizinisch-technischen Bereiches.',
        2017, 'Wien', NULL, 96,
        'Telematik; Gesundheitsförderung; Gesundheitswesen; Medizintechnik; Informatik; Technologischer Wandel; Demografischer Wandel; Finanzierung; Arbeitsmarktentwicklung; Qualifikationsstruktur; Augenoptiker; Ausbildungsberuf; Chirurgiemechaniker; Gesundheitsberuf; Hörgeräteakustiker; Orthopädieschuhmacher; Pflegerischer Beruf; Zahntechniker; Studiengang; Akademisierung; Österreich');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Ausserschulische MINT-Angebote in der Schweiz.', 2017, 'Bern',
        'Akademien der Wissenschaft Schweiz', 56,
        'Außerunterrichtliche Aktivität; Informatik; Mathematik; Naturwissenschaften; Ausbildungsangebot; Exakte Wissenschaft; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Bildung auf einen Blick 2017.', 2017, 'Bielefeld', 'Bertelsmann', 567,
        'Bildung; Bildungschance; Chancengleichheit; Bildungssystem; Bildungsertrag; Schulwesen; Sekundarbereich; Bildungsabschluss; Ausgaben; Beschäftigungseffekt; Berufliche Integration; Berufseinmündung; Niedrig Qualifizierter; Qualifikationsniveau; Ausbildungsabschluss; Ausbildungszeit; Altern; Internationaler Vergleich; Alter; Geschlechtsspezifik; Hochschulabsolvent; OECD (Organisation für wirtschaftliche Zusammenarbeit und Entwicklung); Jugendlicher; Junger Erwachsener; Argentinien; Australien; Belgien; Brasilien; Chile; China; Dänemark; Deutschland; Estland; Finnland; Frankreich; Griechenland; Großbritannien; Indien; Indonesien; Irland; Island; Israel; Italien; Japan; Kanada; Kolumbien; Lettland; Luxemburg; Mexiko; Neuseeland; Niederlande; Norwegen; Österreich; Polen; Portugal; Russland; Saudi-Arabien; Schweden; Schweiz; Slowakei; Slowenien; Spanien; Südafrika (Staat); Südkorea; Tschechien; Türkei; Ungarn; USA');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Gehrke, Birgit; John, Katrin; Kerst, Christian; Wieck, Markus; Sanders, Sandra; Winkelmann, Gert', NULL,
        'Bildung und Qualifikation als Grundlage der technologischen Leistungsfähigkeit Deutschlands 2017.', 2017,
        'Göttingen; Hannover', NULL, 148,
        'Kompetenz; Bildungsbeteiligung; Bildungsverlauf; Bildungssystem; Durchlässigkeit; Informatik; Mathematik; Naturwissenschaften; Wettbewerbsfähigkeit; Ausbildung; Berufsausbildung; Arbeitskräfteangebot; Arbeitskräftebedarf; Arbeitsmarktpolitik; Berufsbildungssystem; Berufsverlauf; Beschäftigungsstruktur; Erwerbstätiger; Qualifikationsstruktur; Hochschulbildung; Bologna-Prozess; Hochschulreform; Studienberechtigter; Dualer Studiengang; Weiterbildungsnachfrage; Dritter Bildungsweg; Internationaler Vergleich; Technik; Hochschulabsolvent; OECD (Organisation für wirtschaftliche Zusammenarbeit und Entwicklung); Absolvent; Studienanfänger; Standardklassifikation für Bildung (ISCED); Europa');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kluxen-Pyta, Donate', NULL, 'Bildungsbedarf für den digitalisierten Arbeitsmarkt.', 2017, 'Berlin', NULL, 8,
        'Medienkompetenz; Allgemein bildendes Schulwesen; Lehrer; Schulbildung; Lehrplan; Computerkenntnisse; Informatik; Technologische Entwicklung; Beschäftigungseffekt; Berufsbildung; Qualifikationsanforderung; Qualifikationsbedarf; Ausbildungsordnung; On-the-Job-Training; Studium; Weiterbildungsbedarf; Auswirkung; Kompetenzentwicklung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Auferkorte-Michaelis, Nicole (Hrsg.); Gillert, Arne (Hrsg.)',
        'ChanceMINT.NRW - Studienbiografische Wendepunkte und Karriereperspektiven.', 2017, 'Opladen', 'Budrich', 144,
        'Motivationsförderung; Frau; Frauenförderung; Informatik; Mathematik; Ingenieurwissenschaft; Naturwissenschaften; Public Private Partnership; Beruf; Berufsverlauf; Berufsorientierung; Berufspraktikum; Studium; Studienverlauf; Studienmotivation; Hochschule; Best-Practice-Modell; Geschlechterverteilung; Technik; Theorie-Praxis-Beziehung; Student; Wissenschaftler; Nordrhein-Westfalen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Müller, Dorothea', NULL, 'Der Berufswahlprozess von Informatiklehrkräften.', 2017, 'Potsdam',
        'Universitätsverlag Potsdam', 299,
        'Bildungsforschung; Medienpädagogik; Lehrerbildung; Unterricht; Informatik; Mangelberuf; Berufswahl; Deutschland; Nordrhein-Westfalen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Die Fachkräftesituation bei Ingenieurinnen und Ingenieuren.', 2017, 'Zürich', 'economiesuisse', 20,
        'Informatik; Mathematik; Naturwissenschaften; Arbeitsmarkt; Ingenieur; Weiterbildung; Forschungsbericht; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL,
        'Arnold, Marlen (Hrsg.); Zawacki-Richter, Olaf (Hrsg.); Haubenreich, Jutta (Hrsg.); Röbken, Heinke (Hrsg.); Götter, Roman (Hrsg.)',
        'Entwicklung von wissenschaftlichen Weiterbildungsprogrammen im MINT-Bereich.', 2017, 'Münster; New York',
        'Waxmann', 482,
        'Programm; Programmplanung; Informatik; Mathematik; Naturwissenschaften; Master-Studiengang; Bachelor-Studiengang; Blended Learning; E-Learning; Weiterbildungsangebot; Wissenschaftliche Weiterbildung; Zielgruppenarbeit; Anerkennung; Bedarfsanalyse; Konzeption; Online-Kurs; Technik');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Handreichung Medien und Informatik.', 2017, 'Chur', 'Amt für Volksschule und Sport', 39,
        'Schule; Curriculum; Computerunterstützter Unterricht; Digitalisierung; Hardware; Konzept; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Dauchert, Annett; Krempkow, René; Krume, Julia; Meyer-Guckel, Volker; Schneider, Melanie; Schröder-Kralemann, Ann-Katrin; Winde, Mathias; Hieronimus, Solveigh; Klier, Julia; Nowak, Sophie; Schreiber, Vanessa; Schröder, Jürgen; Sönmez, Neslihan Ana',
        NULL, 'Hochschul-Bildungs-Report 2020.', 2017, 'Essen; Düsseldorf', NULL, 107,
        'Bildung; Kompetenz; Bildungschance; Chancengleichheit; Bildungssystem; Durchlässigkeit; Lehrer; Lernen; Lerninhalt; Curriculumentwicklung; Arbeitswelt; Digitalisierung; Informatik; Mensch-Maschine-System; Mathematik; Naturwissenschaften; Technologischer Wandel; Reformpolitik; Nachhaltige Entwicklung; Arbeit; Qualifikationsanforderung; Qualifikationswandel; Tätigkeitswandel; Hochschulbildung; Studium; Hochschulpolitik; Hochschuldidaktik; Berufsbegleitendes Studium; Studiengang; Akademiker; Hochschulsystem; Kompetenzentwicklung; Technik; Zukunft; Hochschulabsolvent');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Hochschulpakt 2020 - Umsetzung in der zweiten Projektphase 2011-2015.', 2017, 'Bonn',
        'Gemeinsame Wissenschaftskonferenz (GWK)', 38,
        'Frau; Finanzierung; Fachhochschule; Studentenzahl; Professur; Statistik; Hochschule; Betreuung; Bericht; Rekrutierung; Personal; Student; Studienanfänger; Baden-Württemberg; Bayern; Berlin; Brandenburg; Bremen; Deutschland; Hamburg; Hessen; Mecklenburg-Vorpommern; Niedersachsen; Nordrhein-Westfalen; Rheinland-Pfalz; Saarland; Sachsen; Sachsen-Anhalt; Schleswig-Holstein; Thüringen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Weich, Miriam', NULL, 'Hochschultypen und duales Studium.', 2017, 'Tübingen',
        'Universitätsbibliothek Tübingen', 195,
        'Schlüsselqualifikation; Studium; Dissertation; Dualer Studiengang; Hochschule');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Winde, Mathias (Mitarb.); Schröder, Jürgen (Mitarb.)', 'Höhere Chancen durch höhere Bildung?', 2017,
        'Essen', NULL, 107,
        'Bildung; Chancengleichheit; Neue Medien; Lehrerbildung; Digitalisierung; Informatik; Hochschulbildung; Studium; Defizit; Selektion; Technik; Flüchtling; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Informatik am Gymnasium.', 2017, 'Bern', 'EDK', 13,
        'Schweizerische Konferenz der Kantonalen Erziehungsdirektoren; Sekundarstufe II; Lehrfach; Maturität; Informatik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Müller, Dorothee', NULL, 'Informatikunterricht und Informatikselbstkonzept.', 2017, 'Wuppertal',
        'Universitätsverlag', 19,
        'Bildungsforschung; Selbstkonzept; Lehrerbedarf; Schülerzahl; Gymnasiale Oberstufe; Lehrer; Informatikunterricht; Deutschland; Nordrhein-Westfalen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Informationen zur Umsetzung des Modullehrplans Medien und Informatik.', 2017, 'Frauenfeld',
        'Amt für Volksschule', 13, 'Medien; Curriculum; Informatik; Ausstattung; Informationstechnologie; Thurgau');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'IT-Fachleute.', 2017, 'Nürnberg', NULL, 17,
        'Informatik; Beschäftigungsentwicklung; Arbeitskräftebedarf; Arbeitskräftemangel; Arbeitslosenquote; Arbeitsplatzangebot; Mangelberuf; Stellenbesetzung; Fachkraft; Informationstechnischer Beruf; Programmierer; Hochschulabschluss; Hochschulabsolvent; Studienanfänger');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Kantonaler Hochschulbericht.', 2017, 'Sitten', 'Dienststelle für Hochschulwesen', 62,
        'Informatik; Mathematik; Naturwissenschaften; Arbeitsmarkt; Gesundheitsberuf; Hochschule; Bericht; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Kantonales Konzept für die Integration von Medien und IKT [Medien, Bilder, Informations- und Kommunikationstechnologien] in den Unterricht 2017-2021.',
        2017, 'Freiburg', 'EKSD', 27, 'Medienerziehung; Informatik; Konzept; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Mieg, Harald; Deicke, Wolfgang; Wessels, Insa; Rueß, Julia; Gess, Christopher; Dehne, Julian; Favella, Gianpiero; Heudorfer, Anna; Lucke, Ulrike; Lübcke, Eileen; Reinmann, Gabi; Schiefner-Rohs, Mandy; Danielsiek, Holger; Hubwieser, Peter; Krugel, Johannes; Magenheim, Johannes; Ohrndorf, Laura; Ossenschmidt, Daniel; Schaper, Niclas; Vahrenhold, Jan; Hartz, Stefanie; Aust, Kirsten; Marx, Sabine; Bochmann, René; Koczielsi, Conrad; Baumeister, Antonia; Rindermann, Heiner; Fries, Stefan; Grunschel, Carola; Ebner-Priemer, Ulrich; Nett, Ulrike; Prang, Bianca; Bischoff, Franziska',
        'Hanft, Anke (Hrsg.); Bischoff, Franziska (Hrsg.); Prang, Bianca (Hrsg.)', 'Lehr-/Lernformen.', 2017,
        'Oldenburg', NULL, 74, 'Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Medien und Informatik.', 2017, 'Luzern', 'Dienststelle Volksschulbildung', 13,
        'Medienerziehung; Schule; Computerunterstützter Unterricht; Digitalisierung; Hardware; Konzept; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Medien und Informatik in der Volksschule.', 2017, 'St. Gallen', 'Amt für Volksschule', 16,
        'Medienerziehung; Informatik; Konzept; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Gokus, Susanne (Mitarb.)', 'MINT Nachwuchsbarometer 2017.', 2017, 'München; Hamburg', NULL, 86,
        'Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Koppel, Oliver; Plünnecke, Axel', NULL, 'MINT-Frühjahrsreport 2017.', 2017, 'Köln', NULL,
        104,
        'Bildungsexpansion; Frau; Informatik; Mathematik; Naturwissenschaften; Erwerbsstatistik; Erwerbstätigkeit; Arbeitslosigkeit; Arbeitsplatzangebot; Erwerbstätiger; Berufsprofil; Fachkraft; Innovation; Regionalverteilung; Technik; Älterer Arbeitnehmer; Migrant; Sozialversicherungspflichtiger Arbeitnehmer');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Projektevaluation Medienbildung 2017.', 2017, 'Luzern', 'BKD', 19,
        'Evaluation; Medienerziehung; Projekt; Informatik; Internet; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Brügge, Bernd (Hrsg.); Krusche, Stephan (Hrsg.)', 'Software Engineering im Unterricht der Hochschulen.',
        2017, 'Aachen', NULL, 109, 'Lehre; Software; Informatik; Technologie; Hochschullehrer; Hochschule; Kongress');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Disterer, Georg', NULL, 'Absolventen/innen der Wirtschaftsinformatik zu Erfahrungen im Berufsleben.', 2016,
        'Hannover', 'Hochschule Hannover', 29,
        'Zufriedenheit; Arbeitswelt; Wirtschaftsinformatik; Arbeit; Berufserfahrung; Studium; Bachelor-Studiengang; Hochschule; Absolvent; Online-Publikation; Deutschland; Hannover');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Beiträge zum Mathematikunterricht 2016. Vorträge auf der 50. Tagung für Didaktik der Mathematik vom 07.03.2016 bis 11.03.2016 in Heidelberg. Band 1.',
        2016, 'Münster', 'WTM', 524,
        'Sekundarstufe II; Grundschule; Gymnasium; Lehramtsstudent; Lehramtsstudium; Lehrerausbildung; Lehrerbildung; Lehrerfortbildung; Fachdidaktik; Mathematik; Mathematikunterricht; Studium; Universität; Übergang Schule - Hochschule; Hochschuldidaktik; Konferenz; Konferenzbericht; Konferenzschrift');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kuse, Stefan', 'Müller, Heiko (Mitarb.); Schiefer, Simon (Mitarb.)', 'Berufsausbildung in Hessen 2016.', 2016,
        'Wiesbaden', NULL, 90,
        'Bildungschance; Bildungsstatistik; Schulabschluss; Ausbildung; Berufsausbildung; Berufsbildung; Arbeitsloser Jugendlicher; Berufsgruppe; Berufsstatistik; Qualifikationsstruktur; Ausbildungsbereitschaft; Ausbildungsplatz; Ausbildungsplatzangebot; Ausbildungsplatznachfrage; Ausbildungsvertrag; Duales Ausbildungssystem; Betriebliche Berufsausbildung; Überbetriebliche Ausbildung; Ausbildungsplatzvermittlung; Defizit; Geschlechterverteilung; Prognose; Quote; Regionalverteilung; Ausländer; Jugendlicher; Schulabgänger; Hessen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Orth, Anja Katrin; Plünnecke, Axel', NULL, 'Bildungsmonitor 2016.', 2016, 'Köln', NULL, 237,
        'Bildung; Bildungschance; Chancengleichheit; Zufriedenheit; Bildungssystem; Schule; Erfolgskontrolle; Schulleistung; Übergang Schule - Beruf; Informatik; Mathematik; Naturwissenschaften; Armut; Bundesland; Ausgaben; Personalauswahl; Beruf; Berufschance; Nachwuchs; Fachkraft; Studium; Bologna-Prozess; Hochschulzugang; Studiendauer; Master-Studiengang; Akademisierung; Bachelor-Studiengang; Abbruch; Bekämpfung; Effizienz; Integration; Qualität; Regionaler Vergleich; Technik; Hochschulabsolvent; Flüchtling; Student');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Hinz, Holger; Kimms, Alf', NULL, 'Btx PC-Verbund im Hochschulunterricht.', 2016, 'Kiel; Hamburg', 'ZBW', 23,
        'Finanzwirtschaft; Studium; Hochschullehre; Betriebswirtschaftslehre; Hochschule; Arbeitspapier; Managementinformationssystem');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Der Arbeitsmarkt in Deutschland: MINT-Berufe, März 2016.', 2016, 'Nürnberg', NULL, 37,
        'Frau; Informatik; Mathematik; Naturwissenschaften; Arbeitslosigkeit; Arbeitsmarktchance; Beruf; Berufswahl; Männerberuf; Studienwahl; Technik; Mann; Sozialversicherungspflichtiger Arbeitnehmer');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Der Arbeitsmarkt für IT-Fachleute in Deutschland.', 2016, 'Nürnberg', NULL, 17,
        'Informatik; Beschäftigungsentwicklung; Arbeitskräftebedarf; Arbeitskräftemangel; Arbeitslosigkeit; Arbeitsmarktentwicklung; Arbeitsplatzangebot; Fachkraft; Informatiker; Informationstechnischer Beruf; Studium; Quote; Regionaler Vergleich; Sektorale Verteilung; Berufsnachwuchs; Studienanfänger');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Bünning, Frank; Lehmann, Juliane', NULL,
        'Einfluss von außerschulischen Lernorten auf die Gestaltung von technisch geprägten Karrierewegen.', 2016,
        'Magdeburg',
        'Otto-von-Guericke-Universität, Fakultät für Humanwissenschaften, Institut I - Berufs- und Betriebspädagogik',
        29,
        'Empirische Untersuchung; Interview; Qualitative Forschung; Bildungsbiografie; Bildungsangebot; Schüler; Lernort; Außerschulische Tätigkeit; Außerschulisches Lernen; Informatik; Berufswahl; Einflussfaktor; Technik; Typologie; Wirkung; Wirkungsforschung; Absolvent; Deutschland; Sachsen-Anhalt');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Ohrndorf, Laura', NULL,
        'Entwicklung und Validierung eines Instruments zur Messung des Wissens über Fehlvorstellungen in der Informatik.',
        2016, 'Paderborn', 'Universitätsbibliothek', 227,
        'Wissen; Fehlvorstellung; Fachdidaktik; Informatikunterricht; Dissertation; Instrument; Leistungsmessung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Fachkräftemangel.', 2016, 'Bern', 'SBFI', 17,
        'Informatik; Berufsbildung; Arbeitsmarkt; Berufliche Qualifikation; Gesundheitsberuf; Hotel- und Gaststättengewerbe; Ingenieur; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Streppelhoff, Robin; Tuppi, Julia', NULL, 'Forschungs- und Betreuungsprojekte im deutschen Schwimmsport.',
        2016, 'Bonn', 'Bundesinstitut für Sportwissenschaft', 124,
        'Empirische Forschung; Wissenschaftliche Begleitung; Fachdidaktik; Biomechanik; Projekt; Informatik; Schwimmen; Sport; Sportmedizin; Sportökonomie; Sportpädagogik; Sportpolitik; Sportpsychologie; Sportsoziologie; Sportunterricht; Springen; Trainingswissenschaft; Wasserball; Wasserspringen; Forschungsstand; Bundesinstitut für Sportwissenschaft; Dokumentation; Forschungsprojekt; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Hinweise zur ICT und zum Fachbereich Medien und Informatik an den Volksschulen des Kantons Nidwalden.', 2016,
        'Stans', 'Amt für Volksschulen und Sport', 4,
        'Schule; Curriculum; Computerunterstützter Unterricht; Digitalisierung; Hardware; Konzept; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Horndasch, Sebastian; Jorzik, Bettina; Kanig, Annett; Meyer-Guckel, Volker; Schröder-Kralemann, Ann-Katrin; Winde, Mathias; Haugwitz, Philipp; Hieronimus, Solveigh; Klier, Julia; Kuhlmann, Marianne; Schirmers, Lisa; Schröder, Jürgen; Sönmez, Neslihan Ana; Weihmann, Sophie',
        NULL, 'Hochschulbildung für die Arbeitswelt 4.0.', 2016, 'Essen',
        'Stifterverband für die Deutsche Wissenschaft', 78,
        'Bildung; Kompetenz; Bildungschance; Chancengleichheit; Bildungssystem; Durchlässigkeit; Lehrer; Lernen; Lerninhalt; Curriculumentwicklung; Arbeitswelt; Indikator; Digitalisierung; Informatik; Mensch-Maschine-System; Mathematik; Naturwissenschaften; Technologische Entwicklung; Reformpolitik; Nachhaltige Entwicklung; Arbeit; Beruf; Qualifikationsanforderung; Qualifikationswandel; Tätigkeitswandel; Hochschulbildung; Studium; Hochschulpolitik; Hochschuldidaktik; Berufsbegleitendes Studium; Studiengang; Akademiker; Hochschulsystem; Auswirkung; Kompetenzentwicklung; Quote; Technik; Zukunft; Hochschulabsolvent; Ausländer; Student');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'ICT-Lehrbeginner/innen Befragung 2016.', 2016, 'Bern', 'ICT Berufsbildung Schweiz', 39,
        'Grundbildung; Zufriedenheit; Informatik; Berufsbildung; Berufswahl; Berufsmaturität; Statistik; Forschungsbericht; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Pfeiffer, Sabine; Lee, Horan; Zirnig, Christopher; Suphan, Anne', NULL, 'Industrie 4.0 - Qualifizierung 2025.',
        2016, 'Frankfurt, Main', NULL, 148,
        'Erhebung; Verhalten; Informatik; Mathematik; Maschinenbau; Naturwissenschaften; Technologie; Technologische Entwicklung; Ausbildung; Beruf; Qualifikationsanforderung; Qualifikationsbedarf; Anlagenbau; Gewerblich-technischer Beruf; Betriebliche Berufsausbildung; Betriebliche Weiterbildung; Ausbildungsquote; Weiterbildungsverhalten; Auswirkung; BIBB/BAuA-Erhebung; Produktion; Prognose; Szenarium; Technik; Vernetzung; Bundesinstitut für Berufsbildung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Renger, Anke', NULL,
        'iScience Konzeptentwicklung für Bürgerlabore (Citizen Labs) auf Basis von Schülerlaboren und Citizen Science.',
        2016, 'Berlin', NULL, 112,
        'Forschung; Lernform; Forschendes Lernen; Methodik; Wissensvermittlung; Außerschulischer Lernort; Informatik; Mathematik; Naturwissenschaften; Bürgerbeteiligung; Partizipation; Wissenschaft; Erwachsenenbildung; Konzeptentwicklung; Technik');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kaul, Ashok; Neu, Nathalie; Otto, Anne; Schieler, Manuel', NULL,
        'Karrierestart, Mobilität und Löhne von Absolventen der Informatik.', 2016, 'Nürnberg', NULL, 41,
        'Bildungsertrag; Regionale Mobilität; Informatik; Lohnhöhe; Arbeitsplatzwahl; Berufliche Mobilität; Berufseinmündung; Regionaler Arbeitsmarkt; Fachkraft; Informatiker; Studium; Universität; Studienortwahl; Abwanderung; Bedarf; Ziel; Hochschulabsolvent; Student; Saarland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Konzept zur Einführung des Modullehrplans ''Medien und Informatik'' des Lehrplans 21 in der Volksschule.',
        2016, 'Schwyz', 'Amt für Volksschulen und Sport', 35, 'Medien; Curriculum; Informatik; Konzept; Schwyz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Bergner, Nadine', NULL,
        'Konzeption eines Informatik-Schülerlabors und Erforschung dessen Effekte auf das Bild der Informatik bei Kindern und Jugendlichen.',
        2016, 'Aachen', 'Universitätsbibliothek der RWTH Aachen', 471,
        'Akzeptanz; Fachdidaktik; Informatikunterricht; Schülerexperiment; Dissertation');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Medien und Informatik in der Volksschule.', 2016, 'Bern', 'Erziehungsdirektion des Kantons Bern',
        14, 'Massenmedien; Medienerziehung; Digitalisierung; Informatik; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Medien- und ICT-Empfehlungen für die gemeindlichen Schulen im Kanton Zug.', 2016, 'Zug',
        'Bildungsdirektion', 60,
        'Leitbild; Medienerziehung; Informatik; Datenschutz; Konzept; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Mobile Geräte - eine Orientierungshilfe.', 2016, 'Frauenfeld', 'Amt für Volksschule', 36,
        'Medienerziehung; Computer; Informatik; Konzept; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Naturwissenschaft und Technik: für die Schweiz ein Muss.', 2016, 'Zürich', 'economiesuisse', 21,
        'Geschlechtsunterschied; Förderung; Interesse; Curriculum; Unterricht; Informatik; Informatikunterricht; Mathematik; Mathematikunterricht; Naturwissenschaften; Naturwissenschaftlicher Unterricht; Arbeitsmarkt; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Koppel, Oliver', NULL, 'Physikerinnen und Physiker im Beruf: Anschlussstudie für die Jahre 2005 bis 2013.',
        2016, 'Köln', NULL, 64,
        'Altersstruktur; Bundesland; Beschäftigungsentwicklung; Arbeitskräftebedarf; Arbeitslosigkeit; Arbeitsmarktentwicklung; Ausgeübter Beruf; Berufliche Flexibilität; Berufliche Stellung; Befristeter Arbeitsvertrag; Fachkraft; Physiker; Unbefristeter Arbeitsvertrag; Promotion; Ausländeranteil; Geschlechterverteilung; Regionalverteilung; Sektorale Verteilung; Tätigkeitsfeld');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Sitzmann, Daniel', NULL,
        'Rahmenwerk für zielgruppenorientiertes Blended E-Learning im MINT-Bereich im Kontext des Lebenslangen Lernens.',
        2016, 'Clausthal-Zellerfeld', 'Universitätsbibliothek Clausthal', 432,
        'Lernen; Mathematisch-naturwissenschaftlicher Unterricht; Dissertation; Blended Learning; E-Learning; Lebenslanges Lernen; Internet');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Winkler, Birgit; Gruber, Benjamin', 'Schmid, Kurt (Proj.leit.)', 'Skills for today.', 2016, 'Wien', NULL, 126,
        'Informatik; Mathematik; Naturwissenschaften; Personalbeschaffung; Beschäftigungsentwicklung; Beruf; Beschäftigungsstruktur; Leiharbeitnehmer; Qualifikation; Qualifikationsbedarf; Qualifikationsstruktur; Stellenbesetzung; Technischer Beruf; Technik; Österreich');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Dreher, Ralph (Hrsg.); Jenewein, Klaus (Hrsg.); Neustock, Ulrich (Hrsg.); Schwenger, Ulrich (Hrsg.)',
        'Wandel der technischen Berufsbildung: Ansätze und Zukunftsperspektiven.', 2016, 'Bielefeld',
        'W. Bertelsmann Verlag', 298,
        'Curriculum; Informatik; Maschinenbau; Energiewirtschaft; Berufsbildung; Professionalisierung; Berufsorientierung; Fachschule; Elektrotechnik; Technischer Beruf; Hochschulbildung; Dualer Studiengang; Theorie-Praxis-Beziehung; Berufskolleg; Lehrpersonal; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Seidel, Tina; Reinhold, Sarah; Holzberger, Doris; Mok, Sog Yee; Schiepe-Tiska, Anja; Reiss, Kristina', NULL,
        'Wie gelingen MINT-Schulen? Anregungen aus Forschung und Praxis.', 2016, 'Münster; New York', 'Waxmann', 36,
        'Empirische Forschung; Schwerpunktbildung; Informatikunterricht; Mathematisch-naturwissenschaftlicher Unterricht; Naturwissenschaftlich-technischer Unterricht; Erfahrungsbericht; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Volz Zumbrunnen, Chantal; Notter, Patricia', NULL,
        'Arbeitsmarktanalyse Informatikpraktikerin/Informatikpraktiker EBA.', 2015, 'Zollikofen', 'EHB', 71,
        'Grundbildung; Informatik; Berufsbildung; Arbeitsmarkt; Berufsentwicklung; Ausbildungsangebot; Eidgenössisches Berufsattest; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Stauffer, Martin; Hubacher, Manuel S.; Zitelmann, Reto', NULL, 'Bildungsbericht 2015.', 2015, 'Liestal',
        'Kanton Basel-Landschaft', 50,
        'Übergang Primarstufe - Sekundarstufe I; Übergang Sekundarstufe I - Sekundarstufe II; Leistungsbeurteilung; Fremdsprache; Informatik; Mathematik; Naturwissenschaften; Finanzierung; Übergang Sekundarstufe II - Tertiärstufe; Sonderpädagogik; Bildungsbericht; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Der Arbeitsmarkt für IT-Fachleute in Deutschland.', 2015, 'Nürnberg', NULL, 16,
        'Informatik; Beschäftigungsentwicklung; Arbeitskräftebedarf; Arbeitskräftemangel; Arbeitslosigkeit; Arbeitsmarktentwicklung; Arbeitsplatzangebot; Fachkraft; Informatiker; Informationstechnischer Beruf; Studium; Entwicklung; Quote; Regionaler Vergleich; Sektorale Verteilung; Berufsnachwuchs; Studienanfänger');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Dynamische Strategie Medien und Informatik.', 2015, 'Frauenfeld', 'Amt für Volksschule', 11,
        'Medien; Digitalisierung; Informatik; Thurgau');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Tran, Chat', NULL, 'Elementarisierte Zugänge zur Physik der regenerativen Energie.', 2015, 'Siegen',
        'Universitätsbibliothek der Universität Siegen', 192,
        'Experiment; Projektunterricht; Lehrbedingungen; Fächerübergreifender Unterricht; Erneuerbare Energie; Physik; Physikunterricht; Dissertation; Simulation; Vietnam');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Büchner, Steffen', NULL,
        'Ermittlung zentraler Konzepte der Systemarchitektur eingebetteter Systeme anhand eines fachdidaktisch begründeten Kriterienkataloges.',
        2015, 'Siegen', 'Universitätsbibliothek der Universität Siegen', 208,
        'Fachdidaktik; Dissertation; Kriterienkatalog; System');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Leonhardt, Thiemo', NULL,
        'Etablierung eines begabungsfördernden Lernumfeldes für Mädchen im Bereich Informatik.', 2015, 'Aachen',
        'Techn. Hochsch.', 331,
        'Einstellung (Psy); Selbstkonzept; Selbstwirksamkeit; Junge; Geschlechtsspezifischer Unterschied; Fachdidaktik; Informatik; Informatikunterricht; Intervention; Technik; Mädchen; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Hetze, Pascal', NULL, 'Fachkräftenachwuchs.', 2015, 'Essen', NULL, 11, 'Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Streppelhoff, Robin; Tuppi, Julia', NULL, 'Forschungs- und Betreuungsprojekte im deutschen Schwimmsport.',
        2015, 'Bonn', 'Bundesinstitut für Sportwissenschaft', 274,
        'Empirische Forschung; Wissenschaftliche Begleitung; Sportstätte; Didaktik; Biomechanik; Informatik; Schwimmen; Sport; Sportgeschichte; Sportmedizin; Sportökonomie; Sportpädagogik; Sportpolitik; Sportpsychologie; Sportsoziologie; Sportunterricht; Trainingswissenschaft; Forschungsstand; Bundesinstitut für Sportwissenschaft; Forschungsprojekt; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Frauen in nichtakademischen MINT-Berufen.', 2015, 'Berlin', NULL, 24,
        'Frau; Informatik; Mathematik; Naturwissenschaften; Arbeitszeitgestaltung; Arbeitsbedingungen; Erwerbstätigkeit; Lohnhöhe; Beschäftigungsentwicklung; Arbeitslosenquote; Arbeitslosigkeit; Arbeitsplatz; Beruf; Berufschance; Facharbeiter; Fachkraft; Informationstechnischer Beruf; Meister; Technischer Beruf; Bachelor-Studiengang; Entwicklung; Geschlechterverteilung; Qualität; Technik; Auszubildender; Techniker');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Putz, Sabine; Stockhammer, Hilde; Kaucic-Rieger, Doris; Tschenett, Roswita; Scambor, Elli; Gregoritsch, Petra; Bock-Schappelwein, Julia; Famira-Mühlberger, Ulrike; Horvath, Thomas; Huemer, Ulrike; Schappelwein, Elisabeth; Ziegler, Petra; Papouschek, Ulrike; Mairhuber, Ingrid; Kasper, Ruth',
        'Sturm, René (Hrsg.)', 'Geschlecht, Berufswahl und Arbeitsmarkt.', 2015, 'Wien', NULL, 72,
        'Junge; Frau; Gleichstellung; Frauenförderung; Erfolgskontrolle; Segmentierung; Informatik; Mathematik; Naturwissenschaften; Gleichstellungspolitik; Arbeitsmarkt; Arbeitsmarktpolitik; Beruf; Bildungsadäquate Beschäftigung; Erwerbsbeteiligung; Ausbildungswahl; Berufsberatung; Berufsorientierung; Berufswahl; Pflegerischer Beruf; Geschlechtsspezifik; Technik; Absolvent; Älterer Arbeitnehmer; Mann; Mädchen; Männlicher Jugendlicher; Weibliche Jugendliche; Österreich');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Jaschke, Steffen', NULL, 'Informatikdidaktische Diskussion über das Design eingebetteter Systeme.', 2015,
        'Siegen', 'Universitätsbibliothek der Universität Siegen', 172,
        'Didaktik; Programmierung; Informatik; Dissertation; Diskussion');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Informatische Bildung.', 2015, 'Solothurn', 'Volksschulamt', 52,
        'Medienerziehung; Curriculum; Computeranwendung; Computerunterstützter Unterricht; Software; Informatik; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Dörge, Christina', NULL, 'Informatische Schlüsselkompetenzen.', 2015, 'Potsdam', 'Universität Potsdam', 459,
        'Allgemeinbildung; Kompetenz; Inhaltsanalyse; Kompetenzorientierung; Fachdidaktik; Informatik; Informatikunterricht; Informationstechnik; Schlüsselqualifikation; Dissertation; Hochschulschrift');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Andreoli, Laura; Baumberger, Bernadette; Umbach-Daniel, Anja', NULL, 'Ingenieur-Nachwuchs Schweiz 2015.', 2015,
        'Rüschlikon', 'Rütter Soceco', 57,
        'Informatik; Mathematik; Naturwissenschaften; Ingenieur; Technische Berufsmatura; Statistik; Technische Wissenschaften; Technik; Absolvent; Ausländer; Student; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schauer, Carola; Schauer, Hanno', NULL, 'IT an allgemeinbildenden Schulen.', 2015, 'Essen',
        'Institut für Informatik und Wirtschaftsinformatik (ICB), Universität Duisburg-Essen', 49,
        'Allgemein bildende Schule; Lehrplan; Informationstechnik; Arbeitspapier; Forschungsbericht; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Medien und Informatik in der Volksschule.', 2015, 'St. Gallen', 'Bildungsdepartement', 12,
        'Medienerziehung; Computer; Computerunterstützter Unterricht; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Lange, Thomas; Schröder, Susanne; André, Julia; Mayer, Matthias; Hiller, Sylvia; Renn, Ortwin', NULL,
        'MINT Nachwuchsbarometer 2015.', 2015, 'München', 'acatech', 87,
        'Image; Lehrer; Schüler; Fächerwahl; Unterrichtsfach; Informatik; Mathematik; Naturwissenschaften; Berufsausbildung; Arbeitskräftebedarf; Arbeitskräftemangel; Arbeitslosenquote; Beruf; Nachwuchs; Berufsorientierung; Berufswahl; Berufsschule; Fachkraft; Informationstechnischer Beruf; Naturwissenschaftlicher Beruf; Technischer Beruf; Betriebliche Berufsausbildung; Ausbildungsquote; Studienwahl; Geschlechtsspezifik; Technik');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Koppel, Oliver; Plünnecke, Axel', NULL, 'MINT-Herbstreport 2015.', 2015, 'Köln', NULL, 98,
        'Fremdsprachenkenntnisse; Landkreis; Informatik; Mathematik; Naturwissenschaften; Bundesland; Lohnhöhe; Beschäftigungsentwicklung; Arbeitskräftemangel; Beruf; Berufliche Integration; Erwerbspersonenpotenzial; Qualifikationsstruktur; Fachkraft; Personalbedarf; Regionaler Vergleich; Rekrutierung; Technik; Älterer Arbeitnehmer; Einwanderer; Flüchtling; Führungskraft');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Schlussbericht der Arbeitsgruppe zu Medien und Informatik im Lehrplan 21.', 2015, 'Luzern',
        'D-EDK', 39, 'Medienerziehung; Curriculum; Informatik; Bericht; Deutschsprachige Schweiz; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schauer, Carola; Schauer, Hanno', NULL, 'Schulische IT- und Medienbildung.', 2015, 'Essen', 'ICB', 39,
        'Mediennutzung; Weiterführende Schule; Lehrplan; Arbeitspapier; Rheinland-Pfalz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Jüttemann, Michaela', NULL, 'Strukturelle Determinanten geschlechtsspezifischer Segregation an Universitäten.',
        2015, 'Aachen', 'Hochschulbibliothek der Rheinisch-Westfälischen Technischen Hochschule Aachen', 203,
        'Empirische Untersuchung; Determinante; Segregation; Universität; Dissertation; Geschlechtsspezifik; Struktur');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Heublein, Ulrich; Ebert, Julia; Hutzsch, Christopher; Isleib, Sören; Richter, Johanna; Schreiber, Jochen',
        NULL, 'Studienbereichsspezifische Qualitätssicherung im Bachelorstudium.', 2015, 'Hannover', NULL, 75,
        'Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Berthold, Christian; Lah, Wencke; Röwert, Ronny', NULL, '''Und wo studieren die jetzt alle?''', 2015,
        'Gütersloh', NULL, 66, 'Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schulmeister, Sönke', NULL,
        'Unterrichtsentwicklung durch Wettbewerbe - Analyse zweier Informatikwettbewerbe für den Schulunterricht.',
        2015, 'Kiel', 'Universitätsbibliothek Kiel', 249,
        'Unterrichtsentwicklung; Unterricht; Informatik; Wettbewerb; Dissertation; Analyse');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Wie vermeiden wir den ''Numerus clausus'' in der Schweiz?', 2015, 'Zürich', 'economiesuisse', 12,
        'Lernfähigkeit; Maturität; Fächerwahl; Informatik; Mathematik; Naturwissenschaften; Laufbahnberatung; Berufsberatung; Übergang Sekundarstufe II - Tertiärstufe; Numerus clausus; Studienberatung; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Scheuner, Barbara Maria', NULL, 'Analyse eines ''Technical Visual Literacy''-Unterrichts imt E-Observation.',
        2014, 'Zürich', 'ETHZ', 148, 'Informatik; Visualisieren; E-Learning; Datenverarbeitung; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Frank, Carolin', NULL, 'Arbeitswelt als Kontext.', 2014, 'Dresden', 'Technische Universität', 546,
        'Erwartung; Image; Selbstwirksamkeit; Interesse; Unterrichtsgegenstand; Unterricht; Arbeitswelt; Informatik; Mathematik; Naturwissenschaften; Prädiktor; Arbeitsanalyse; Arbeitskräftemangel; Berufsinteresse; Berufsorientierung; Berufswahl; Fachkraft; Studium; Studienwahl; Ingenieurwesen; Berufskonzept; Abbruch; Technik');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Keck Frei, Andrea; Thomann, Geri', NULL, 'Begleitstudie Flipped Classroom ZHAW Informatik.', 2014, 'Zürich',
        'PHZH', 5, 'Evaluation; Tertiäre Bildung; Unterrichtsmethode; Fachhochschule; Hochschulbildung; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Börlin, Johannes; Beerenwinkel, Anne; Labudde, Peter', NULL,
        'Bericht Analyse MINT-Nachwuchsbarometer im Auftrag der Akademien der Wissenschaften Schweiz (a+).', 2014,
        'Basel', 'PH FHNW', 291,
        'Geschlechtsunterschied; Interesse; Informatik; Mathematik; Naturwissenschaften; Forschungsbericht; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kuse, Stefan', 'Müller, Heiko (Mitarb.); Schiefer, Simon (Mitarb.)', 'Berufsausbildung in Hessen 2014.', 2014,
        'Wiesbaden', 'HA Hessen-Agentur', 91,
        'Bildungschance; Bildungsstatistik; Schulabschluss; Migrationshintergrund; Berufsausbildung; Berufsbildung; Arbeitsloser Jugendlicher; Berufsgruppe; Berufsstatistik; Qualifikationsstruktur; Ausbildungsbereitschaft; Ausbildungsplatzangebot; Ausbildungsplatzmangel; Ausbildungsplatznachfrage; Ausbildungsvertrag; Duales Ausbildungssystem; Betriebliche Berufsausbildung; Überbetriebliche Ausbildung; Ausbildungsplatzvermittlung; Geschlechterverteilung; Regionalverteilung; Ausländer; Jugendlicher; Schulabgänger; Hessen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Bildungs-, Forschungs- und Innovationspolitik.', 2014, 'Zürich', 'economiesuisse', 36,
        'Bildungspolitik; Forschungspolitik; Sprachunterricht; Informatik; Mathematik; Naturwissenschaften; Fachhochschule; Qualitätsentwicklung; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Esselmann, Ina; Kemeny, Felicitas; Plünnecke, Axel', NULL, 'Bildungsmonitor 2014.', 2014,
        'Köln', NULL, 199,
        'Bildung; Bildungschance; Bildungsniveau; Chancengleichheit; Bildungssystem; Benachteiligtenförderung; Forschungsaufwand; Bildungsertrag; Kinderbetreuung; Schule; Schülerzahl; Gymnasium; Schulische Integration; Informatik; Lesen; Mathematik; Naturwissenschaften; Bundesland; Internationalisierung; Reformpolitik; Sozioökonomischer Faktor; Ausgaben; Benchmarking; Investition; Öffentliche Ausgaben; Ökonomische Determinanten; Wirtschaftswachstum; Berufsbildung; Arbeitsmarktchance; Beruf; Ausbildungszeit; Berufsvorbereitung; Studiendauer; Akademisierung; Dauer; Effizienz; Kompetenzentwicklung; Qualität; Quote; Regionaler Vergleich; Technik; Hochschulabsolvent');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Der Arbeitsmarkt in Deutschland: MINT-Berufe, Februar 2014.', 2014, 'Nürnberg', NULL, 36,
        'Frau; Informatik; Mathematik; Naturwissenschaften; Arbeitslosigkeit; Arbeitsmarktchance; Beruf; Berufswahl; Männerberuf; Studienwahl; Technik; Mann; Sozialversicherungspflichtiger Arbeitnehmer');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Empfehlungen zur Weiterentwicklung des Hochschulsystems des Saarlandes.', 2014, 'Berlin', NULL,
        168, 'Hochschulpolitik; Hochschule; Empfehlung; Saarland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schneider, Wolfram', NULL,
        'Entwicklung, Umsetzung und Bewertung gendersensibler MINT-Lehr-Lernprozesse in Schule und Universität.', 2014,
        'München', 'Universitätsbibliothek der TU München', 366,
        'Erziehungswissenschaft; Gender; Schule; Lehr-Lern-Prozess; Informatik; Mathematik; Naturwissenschaften; Universität; Dissertation; Bewertung; Entwicklung; Technik');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Ihsen, Susanne; Schiffbänker, Helen; Holzinger, Florian; Jeanrenaud, Yves; Sanwald, Ulrike; Scheibl, Katharina; Schneider, Wolfram',
        NULL, 'Frauen im Innovationsprozess.', 2014, 'Berlin', NULL, 167,
        'Rollenwandel; Rollenverständnis; Frau; Gleichstellung; Informatik; Mathematik; Naturwissenschaften; Gleichstellungspolitik; Quotierung; Beruf; Erwerbsbeteiligung; Frauenerwerbstätigkeit; Studienwahl; Internationaler Vergleich; Innovation; Innovationspotenzial; Technik; Hochschulabsolvent; Führungskraft; Student; Wissenschaftler; Deutschland; Österreich; Rumänien; Schweden; USA');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Hetze, Pascal; Janoschka, Oliver; Jorzik, Bettina; Meyer-Guckel, Volker; Winde, Mathias; Bedürftig, Malte; Bergner, Christiane; Hieronimus, Solveigh; Klier, Julia; Schröder, Jürgen; Sönmez, Neslihan Ana',
        NULL, 'Hochschul-Bildungs-Report 2020.', 2014, 'Essen u.a.', NULL, 72,
        'Bildung; Bildungschance; Chancengleichheit; Bildungssystem; Durchlässigkeit; Lehrer; Lehramt; Informatik; Mathematik; Naturwissenschaften; Beruf; Technischer Beruf; Fachhochschule; Hochschulbildung; Studium; Hochschulpolitik; Studentenzahl; Studienwahl; Dualer Studiengang; Hochschulsystem; E-Learning; Weiterbildungsangebot; Wissenschaftliche Weiterbildung; Dritter Bildungsweg; Bericht; Praxisbezug; Quote; Technik; Hochschulabsolvent; Europäische Union; Abiturient; Ausländer; Bürger; Student; Studienanfänger');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Roos, Markus; u.a.', NULL, 'Implementation des Ergänzungsfachs Informatik an Schweizer Gymnasien.', 2014,
        'Baar', 'Spectrum3', 237, 'Maturität; Ergänzungsfach; Informatik; Forschungsbericht; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kulka, Irena', NULL, 'Informatik auf Erfolgskurs - Bildungsreform am Beispiel Grossbritannien.', 2014, 'Bern',
        'Hasler Stiftung', 9,
        'Bildungsreform; Lehrfach; Curriculum; Curriculumentwicklung; Informatik; Mathematik; Naturwissenschaften; Technik; Schweiz; Vereinigtes Königreich');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Ingenieurberufe in den Medien.', 2014, 'Chur', 'HTW Chur', 25,
        'Massenmedien; Informatik; Bild; Mathematik; Naturwissenschaften; Berufswahl; Ingenieur; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Ingenieurberufe in den Medien.', 2014, 'Chur', 'HTW Chur', 34,
        'Massenmedien; Informatik; Bild; Mathematik; Naturwissenschaften; Berufswahl; Ingenieur; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Ingenieurberufe in den Medien.', 2014, 'Chur', 'HTW Chur', 28,
        'Inhaltsanalyse; Massenmedien; Informatik; Bild; Mathematik; Naturwissenschaften; Ingenieur; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Giesen, Birgit (Hrsg.); Zimmermann, Stefanie (Red.)', 'JobTrends Deutschland 2014.', 2014, 'Köln', NULL,
        75,
        'Informatik; Ingenieurwissenschaft; Naturwissenschaften; Lohnhöhe; Personalauswahl; Unternehmen; Arbeitsmarktchance; Berufseinmündung; Qualifikationsanforderung; Schlüsselqualifikation; Stellenangebot; Bewerbung; Informatiker; Ingenieur; Jurist; Naturwissenschaftler; Rechtswissenschaft; Wirtschaftswissenschaft; Master-Studiengang; Bachelor-Studiengang; Auswahlverfahren; Personalbedarf; Internet; Hochschulabsolvent; Berufsanfänger; Wirtschaftswissenschaftler');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Lehrmittelobligatorium im Fachbereich ''Natur und Technik'' (NT).', 2014, 'Zürich', 'Bildungsrat',
        3, 'Bildung; Lehrmittel; Informatik; Mathematik; Naturwissenschaften; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Rummler, Klaus (Hrsg.)', 'Lernräume gestalten - Bildungskontexte vielfältig denken.', 2014,
        'Münster u.a.', 'Waxmann', 662,
        'Spielerisches Lernen; Digitale Medien; Elektronische Medien; Mediendidaktik; Medieneinsatz; Mediennutzung; Raumgestaltung; Informelles Lernen; Lernumgebung; Software; Soziale Software; Mobilität; Hochschuldidaktik; Hochschule; E-Learning; Konferenz; Tagung; Individuum; Zürich');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Dreyer, Hans Peter', NULL, 'MINT Gymnasium.', 2014, 'Zürich', NULL, 34,
        'Maturität; Informatik; Mathematik; Naturwissenschaften; Forschungsbericht; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Heinze, Christina; Dreyer, Hans Peter', NULL, 'MINT Gymnasium.', 2014, 'Zürich', NULL, 129,
        'Erhebung; Maturität; Informatik; Mathematik; Naturwissenschaften; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Koppel, Oliver; Plünnecke, Axel', NULL, 'MINT-Frühjahrsreport 2014.', 2014, 'Köln', NULL,
        103,
        'Bildungsexpansion; Frau; Informatik; Mathematik; Naturwissenschaften; Erwerbsstatistik; Erwerbstätigkeit; Arbeitskräftebedarf; Arbeitslosigkeit; Arbeitsplatzangebot; Beruf; Erwerbstätiger; Fachkraft; Innovation; Regionalverteilung; Technik; Älterer Arbeitnehmer; Migrant; Sozialversicherungspflichtiger Arbeitnehmer');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'MINT-Nachwuchsbarometer Schweiz.', 2014, NULL, 'Akademien der Wissenschaft Schweiz', 12,
        'Geschlechtsunterschied; Interesse; Informatik; Mathematik; Naturwissenschaften; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Trabert, Lioba; Ramsauer, Kathrin', NULL, 'MINT-Projekte in Hessen - Bestandsaufnahme und Strukturanalyse.',
        2014, 'Wiesbaden', 'HA Hessen-Agentur', 142,
        'Bildungspolitik; Außerschulische Tätigkeit; Projekt; Informatik; Mathematik; Naturwissenschaften; Praktikum; Informationstechnischer Beruf; Naturwissenschaftler; Amtliche Druckschrift; Bestandsaufnahme; Empfehlung; Strukturanalyse; Technik; Hessen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        '''Natur und Technik'' (NT) auf der Kindergartenstufe und Sekundarstufe I. Anforderungskatalog und Konzeptaufträge für neue Lehrmittel.',
        2014, 'Zürich', 'Bildungsrat', 15,
        'Bildung; Lehrmittel; Informatik; Mathematik; Naturwissenschaften; Entwicklung; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Baethge, Martin; Kerst, Christian; Leszczensky, Michael; Wieck, Markus; Gehrke, Birgit', NULL,
        'Neue Konstellation zwischen Hochschulbildung und Berufsausbildung.', 2014, 'Hannover', NULL, 64,
        'Bildungsbeteiligung; Bildungsverlauf; Bildungssystem; Durchlässigkeit; Informatik; Mathematik; Naturwissenschaften; Partizipation; Erwerbstätigkeit; Wettbewerbsfähigkeit; Ausbildung; Berufsausbildung; Arbeitskräfteangebot; Arbeitskräftebedarf; Arbeitsmarkt; Beruf; Berufsbildungssystem; Berufsverlauf; Erwerbstätiger; Qualifikationsstruktur; Hochschulbildung; Bologna-Prozess; Hochschulreform; Studienberechtigter; Dualer Studiengang; Weiterbildung; Dritter Bildungsweg; Internationaler Vergleich; Prognose; Struktur; Technik; Hochschulabsolvent; OECD (Organisation für wirtschaftliche Zusammenarbeit und Entwicklung); Absolvent; Ausländer; Student; Studienanfänger; Europa');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Potenziale des dualen Studiums in den MINT-Fächern.', 2014, 'München', 'Utz', 38,
        'Prüfungsordnung; Maschinenbau; Naturwissenschaften; Ingenieur; Studium; Berufsbegleitendes Studium; Studienverhalten; Praxis; Studentenschaft; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL,
        'Wolter, Andrä (Hrsg.); Kamm, Caroline (Hrsg.); Lenz, Katharina (Hrsg.); Renger, Peggy (Hrsg.); Spexard, Anna (Hrsg.)',
        'Potenziale des dualen Studiums in den MINT-Fächern.', 2014, 'München', 'Utz', 175,
        'Bildung; Eignung; Soziale Herkunft; Frau; Informatik; Mathematik; Naturwissenschaften; Migrationshintergrund; Sozialstruktur; Finanzierung; Ausbildung; Beruf; Berufseinmündung; Studium; Studentenzahl; Studiengebühren; Hochschulzugang; Studierfähigkeit; Berufsbegleitendes Studium; Dualer Studiengang; Studienmotivation; Master-Studiengang; Abbruch; Bewerberauswahl; Praxisbezug; Technik; Theorie-Praxis-Beziehung; Zugangsvoraussetzung; Hochschulabsolvent; Abiturient; Absolvent; Student; Zielgruppe; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Stärkung von Naturwissenschaft und Technik an den Zürcher Mittelschulen: Umsetzung.', 2014,
        'Zürich', 'Bildungsrat', 3,
        'Bildung; Sekundarstufe II; Gymnasium; Maturitätsschule; Informatik; Mathematik; Naturwissenschaften; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Langfeldt, Bettina (Hrsg.); Mischau, Anina (Hrsg.)', 'Strukturen, Kulturen und Spielregeln.', 2014,
        'Baden-Baden', 'Nomos', 273,
        'Kultur; Forschungseinrichtung; Diversity Management; Mobilitätsbarriere; Stereotyp; Frau; Geschlechterrolle; Frauenförderung; Leistungsbeurteilung; Informatik; Determinante; Mathematik; Erfindung; Naturwissenschaften; Physik; Gleichstellungspolitik; Patent; Unternehmenskultur; Beruf; Berufliche Selbstständigkeit; Beruflicher Aufstieg; Berufserfolg; Berufsverlauf; Mathematiker; Physiker; Promotion; Studiengang; Hochschulsystem; Geschlechtsspezifik; Innovationspotenzial; Technik; Institution; Mann; Wissenschaftler');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Güdel, Karin', NULL, 'Technikaffinität von Mädchen und Jungen der Sekundarstufe.', 2014, NULL, NULL, 380,
        'Bildung; Geschlechtsunterschied; Sekundarstufe I; Informatik; Mathematik; Naturwissenschaften; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kleiner, Paul', NULL, 'Was ist Informatik?', 2014, 'Bern', 'Hasler Stiftung', 20,
        'Allgemeinbildung; Maturität; Informatik; Informationstechnologie; Informationswissenschaft; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Klingert, Isabell; Block, Andreas H.', NULL, 'Ausländische Wissenschaftler in Deutschland.', 2013, 'Nürnberg',
        NULL, 67, 'Einwanderung; Arbeitsmarkt; Arbeitsmobilität; Ausländer; Wissenschaftler; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Hielscher, Michael', NULL,
        'Autorenwerkzeuge für digitale, multimediale und interaktive Lernbausteine im Web 2.0.', 2013, 'Mainz',
        'Universitätsbibliothek Mainz', 154,
        'Multimedia; Computerprogramm; Soziale Software; Dissertation; E-Learning; Interaktivität; Lerneinheit');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Baden, Christian; Gericke, Pierre-Andre; Hasberg, Ruth; Klems, Wolfgang; Schmid, Alfons; Werth, Stefan', NULL,
        'Berufliche Qualifikationsmismatches bei Zugängen in Arbeitslosigkeit.', 2013, 'Frankfurt, Main', NULL, 55,
        'Wissen; Lager; Informatik; Mathematik; Naturwissenschaften; Mismatch; Arbeit; Arbeitsloser; Arbeitsplatzverlust; Beruf; Berufsanforderung; Berufskraftfahrer; Qualifikationsanforderung; Unterwertige Beschäftigung; Überqualifikation; Bürofachkraft; Einzelhandelskaufmann; Gesundheitsberuf; Kellner; Koch; Maler; Maurer; Transportberuf; Umweltberuf; Technik; Verwaltung; Ostdeutschland; Westdeutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kuse, Stefan', 'Müller, Heiko (Mitarb.); Schiefer, Simon (Mitarb.)', 'Berufsausbildung in Hessen 2013.', 2013,
        'Wiesbaden', 'HA Hessen-Agentur', 93,
        'Bildungschance; Bildungsstatistik; Schulabschluss; Migrationshintergrund; Berufsausbildung; Berufsbildung; Arbeitsloser Jugendlicher; Arbeitsmarktentwicklung; Berufsgruppe; Berufsstatistik; Qualifikationsstruktur; Ausbildungsbereitschaft; Ausbildungsförderung; Ausbildungsplatzangebot; Ausbildungsplatzmangel; Ausbildungsplatznachfrage; Ausbildungsvertrag; Duales Ausbildungssystem; Betriebliche Berufsausbildung; Überbetriebliche Ausbildung; Ausbildungsplatzvermittlung; Ausbildungsquote; Geschlechterverteilung; Regionalverteilung; Ausländer; Jugendlicher; Schulabgänger; Deutschland; Hessen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Simon, Silvia; Küng, Brigitte; Bau, Frank', NULL,
        'Berufswahlentscheidung Technik: Wie und warum sich Jugendliche für oder gegen handwerkliche und technische Berufe entscheiden.',
        2013, 'Chur', 'HTW', 27,
        'Erhebung; Geschlechtsunterschied; Informatik; Mathematik; Naturwissenschaften; Berufswahl; Handwerk; Technischer Beruf; Gewerblich-industrielle Ausbildung; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Leszczensky, Michael; Cordes, Alexander; Kerst, Christian; Meister, Tanja; Wespel, Johannes', NULL,
        'Bildung und Qualifikation als Grundlage der technologischen Leistungsfähigkeit Deutschlands.', 2013,
        'Hannover', 'HIS', 146,
        'Bildung; Wissensgesellschaft; Bildungsbeteiligung; Bildungsmotivation; Frau; Bildungspolitik; Forschungsförderung; Informatik; Mathematik; Ingenieurwissenschaft; Naturwissenschaften; Technologische Entwicklung; Politik; Finanzierung; Internationaler Wettbewerb; Personalbeschaffung; Strukturwandel; Unternehmen; Wettbewerbsfähigkeit; Wirtschaft; Berufsausbildung; Arbeitskräftebedarf; Arbeitsmarktentwicklung; Arbeitsplatzangebot; Beruf; Erwerbsbeteiligung; Erwerbsbevölkerung; Nachwuchs; Qualifikationsanforderung; Qualifikationsbedarf; Qualifikationsstruktur; Ausbildungsplatzangebot; Berufslenkung; Fachkraft; Naturwissenschaftlicher Beruf; Hochschulbildung; Studium; Studentenzahl; Studienberechtigter; Informationswirtschaft; Akademiker; Weiterbildung; Dauer; Abbruch; Auslandsaufenthalt; Innovationsfähigkeit; Prognose; Quote; Standortfaktor; Technik; Hochschulabsolvent; Europäische Union; Berufsnachwuchs; Hoch Qualifizierter; Studienanfänger; Wissenschaftler');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Leszczensky, Michael; Cordes, Alexander; Kerst, Christian; Meister, Tanja; Wespel, Johannes', NULL,
        'Bildung und Qualifikation als Grundlage der technologischen Leistungsfähigkeit Deutschlands.', 2013,
        'Hannover', 'HIS', 160,
        'Bildung; Forschung; Wissensgesellschaft; Bildungsbeteiligung; Frau; Bildungspolitik; Forschungsförderung; Informatik; Mathematik; Ingenieurwissenschaft; Naturwissenschaften; Technologische Entwicklung; Politik; Finanzierung; Internationaler Wettbewerb; Personalbeschaffung; Strukturwandel; Unternehmen; Wettbewerbsfähigkeit; Wirtschaft; Ausbildung; Berufsausbildung; Arbeitsmarktentwicklung; Arbeitsplatzangebot; Beruf; Erwerbsbeteiligung; Erwerbsbevölkerung; Nachwuchs; Qualifikationsanforderung; Qualifikationsbedarf; Qualifikationsstruktur; Ausbildungsplatzangebot; Berufslenkung; Fachkraft; Naturwissenschaftlicher Beruf; Hochschulbildung; Studium; Wissenschaft; Studentenzahl; Studienberechtigter; Akademiker; Weiterbildung; Dauer; Abbruch; Auslandsaufenthalt; Bedarf; Innovationsfähigkeit; Prognose; Quote; Standortfaktor; Technik; Hochschulabsolvent; Europäische Union; Berufsnachwuchs; Hoch Qualifizierter; Studienanfänger; Wissenschaftler; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Kemeny, Felicitas Stephanie; Plünnecke, Axel', NULL, 'Bildungsmonitor 2013.', 2013, 'Köln',
        NULL, 167,
        'Bildung; Bildungschance; Bildungsniveau; Chancengleichheit; Bildungssystem; Benachteiligtenförderung; Forschungsaufwand; Bildungsertrag; Kinderbetreuung; Schule; Schülerzahl; Schulische Integration; Informatik; Lesen; Mathematik; Naturwissenschaften; Bundesland; Internationalisierung; Sozioökonomischer Faktor; Ausgaben; Benchmarking; Investition; Öffentliche Ausgaben; Ökonomische Determinanten; Wirtschaftswachstum; Berufsbildung; Arbeitsmarktchance; Beruf; Ausbildungszeit; Berufsvorbereitung; Studiendauer; Akademisierung; Effizienz; Kompetenzentwicklung; Qualität; Quote; Regionaler Vergleich; Technik; Hochschulabsolvent');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Martens, Kerstin; Breiter, Andreas; Idel, Till-Sebastian; Knipping, Christine; Teltemann, Janna', NULL,
        'Das ''PISA-Phänomen''.', 2013, 'Bremen', 'Sfb 597 ''Staatlichkeit im Wandel''', 35,
        'Erziehungswissenschaft; Forschung; TIMSS (Third International Mathematics and Science Study); Rezeption; Wahrnehmung; Informatik; Mathematik; Fachwissenschaft; Politikwissenschaft; Soziologie; Interdisziplinarität; Veröffentlichung; IGLU (Internationale Grundschul-Lese-Untersuchung); PISA (Programme for International Student Assessment)');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Der Arbeitsmarkt für Akademikerinnen und Akademiker: Gute Bildung - gute Chancen.', 2013,
        'Nürnberg', NULL, 91,
        'Beschäftigungsentwicklung; Arbeitskräftebedarf; Arbeitslosenquote; Arbeitslosigkeit; Arbeitsmarktentwicklung; Berufseinmündung; Berufsgruppe; Akademiker; Bachelor-Studiengang; Geschlechterverteilung; Personalbedarf; Regionaler Vergleich; Hochschulabsolvent; Sozialversicherungspflichtiger Arbeitnehmer; Ostdeutschland; Westdeutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Der Arbeitsmarkt in Deutschland: Fachkräfteengpassanalyse, Juni 2013.', 2013, 'Nürnberg', NULL, 22,
        'Informatik; Automatisierungstechnik; Energietechnik; Klimatechnik; Maschinenbau; Mechatronik; Bundesland; Arbeitskräftemangel; Arbeitsmarktentwicklung; Arbeitsplatzangebot; Berufsgruppe; Mangelberuf; Berufsfeld; Elektrotechnik; Fachkraft; Fahrzeugbau; Gesundheitsberuf; Informationstechnischer Beruf; Ingenieur; Krankenschwester; Pflegerischer Beruf; Schienenfahrzeugführer; Technischer Beruf; Altenpflege; Laufzeit; Regionaler Vergleich; Ostdeutschland; Westdeutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Demary, Vera; Koppel, Oliver', NULL, 'Die Abgrenzung des mittel- und hochqualifizierten MINT-Segments.', 2013,
        'Köln', NULL, 38,
        'Kompetenz; Informatik; Mathematik; Naturwissenschaften; Ausbildung; Anforderungsprofil; Beruf; Berufsgruppe; Berufsklassifikation; Qualifikation; Berufsfeld; Facharbeiter; Meister; Akademiker; Technik; Hochschulabsolvent; Absolvent; Hoch Qualifizierter; Techniker');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Merkel, Karola', NULL,
        'Eine Informatik-didaktische Methode für das Erlernen von Projektarbeit in Schule und Hochschule.', 2013,
        'Aachen', 'Hochschulbibliothek der Rheinisch-Westfälischen Technischen Hochschule Aachen', 358,
        'Projektmethode; Teamarbeit; Unterrichtsmodell; Fachdidaktik; Informatikunterricht; Dissertation; Aufgabe');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Frauen in MINT-Berufen.', 2013, 'Berlin', NULL, 24,
        'Familie; Mutter; Frau; Informatik; Mathematik; Naturwissenschaften; Erwerbstätigkeit; Beschäftigungsentwicklung; Arbeitskräftemangel; Arbeitslosenquote; Beruf; Beruflicher Aufstieg; Teilzeitbeschäftigung; Chemiker; Fachkraft; Informationstechnischer Beruf; Ingenieur; Mathematiker; Naturwissenschaftler; Physiker; Geschlechterverteilung; Geschlechtsspezifik; Quote; Technik; Hochschulabsolvent; Studienanfänger; Techniker');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Stabler, Jochen', NULL, 'Frauen und Männer in MINT-Berufen im Saarland.', 2013, 'Nürnberg', NULL, 50,
        'Frau; Informatik; Mathematik; Naturwissenschaften; Arbeitslosigkeit; Arbeitsmarktchance; Beruf; Berufswahl; Männerberuf; Studienwahl; Technik; Mann; Sozialversicherungspflichtiger Arbeitnehmer; Saarland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Fünfte Bilanz Chancengleichheit - Chancengleichheit auf einem guten Weg.', 2013, 'Berlin', NULL,
        121,
        'Bildungsbeteiligung; Chancengleichheit; Familie; Frau; Kinderbetreuung; Informatik; Mathematik; Naturwissenschaften; Einkommensunterschied; Gleichstellungspolitik; Erwerbsquote; Erwerbstätigkeit; Personalpolitik; Privatwirtschaft; Berufsausbildung; Beruf; Erwerbsbeteiligung; Erwerbstätiger; Geringfügige Beschäftigung; Teilzeitbeschäftigung; Berufswahl; Unternehmer; Hochschulbildung; Habilitation; Promotion; Studienabschluss; Weiterbildung; Best-Practice-Modell; Technik; Führungskraft');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Koller, Petra (Hrsg.); Meffre, Véronique (Hrsg.)', 'MINT-Fachkräfte auf dem Arbeitsmarkt.', 2013,
        'Neuchâtel', 'Bundesamt für Statistik', 90, 'Berufswahl; Akademiker; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Koller, Petra; Mettre, Véronique', NULL, 'MINT-Fachkräfte auf dem Arbeitsmarkt.', 2013, 'Neuchâtel', 'BFS', 96,
        'Bildungsstatistik; Übergang Schule - Beruf; Informatik; Mathematik; Naturwissenschaften; Mobilität; Arbeitsbedingungen; Ausbildung; Arbeitsmarkt; Berufliche Qualifikation; Statistik; Hochschule; Forschungsbericht; Technik; Absolvent; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Demary, Vera; Koppel, Oliver; Plünnecke, Axel', NULL, 'MINT-Frühjahrsreport 2013.', 2013,
        'Köln', NULL, 105,
        'Informatik; Mathematik; Naturwissenschaften; Altersstruktur; Arbeitsbedingungen; Branche; Einkommenshöhe; Beschäftigungsentwicklung; Ausbildung; Arbeitskräfteangebot; Arbeitskräftebedarf; Arbeitskräftemangel; Arbeitsmarktentwicklung; Beruf; Erwerbstätiger; Informatiker; Mathematiker; Naturwissenschaftler; Akademiker; Geschlechterverteilung; Innovationspotenzial; Prognose; Sektorale Verteilung; Struktur; Technik; Hochschulabsolvent; Absolvent; Arbeitnehmer; Ausländer; Älterer Arbeitnehmer; Einwanderer; Techniker');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Koppel, Oliver; Plünnecke, Axel', NULL, 'MINT-Herbstreport 2013.', 2013, 'Köln', NULL, 94,
        'Informatik; Mathematik; Naturwissenschaften; Altersstruktur; Migration; Arbeitsbedingungen; Branche; Einkommenshöhe; Erwerbstätigkeit; Wirtschaftsprognose; Beschäftigungsentwicklung; Ausbildung; Arbeitskräfteangebot; Arbeitskräftebedarf; Arbeitskräftemangel; Arbeitsmarktentwicklung; Beruf; Informatiker; Mathematiker; Naturwissenschaftler; Akademiker; Geschlechterverteilung; Innovationspotenzial; Sektorale Verteilung; Struktur; Technik; Hochschulabsolvent; Absolvent; Arbeitnehmer; Ausländer; Älterer Arbeitnehmer; Einwanderer; Techniker');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'MINT-Klasse.', 2013, 'Kreuzlingen', 'Kantonsschule', 1,
        'Modellschule; Maturität; Modellversuch; Informatik; Mathematik; Naturwissenschaften; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Schulmeister, Rolf (Hrsg.)', 'MOOCs - Massive Open Online Courses.', 2013, 'Münster u.a.', 'Waxmann',
        275,
        'Digitale Medien; Elektronische Medien; Medieneinsatz; Bildungspolitik; Beispiel; Soziale Software; Geschichte (Histor); Hochschuldidaktik; Seminar; Hochschule; Blended Learning; E-Learning; Kurs; Analyse; Erfahrungsbericht; Online; World Wide Web; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Multicheck Schülerumfrage: die gemeinsame Studie von Multicheck und Forschungsinstitut gfs-zürich: Sonderauswertung ICT Berufsbildung Schweiz.',
        2013, NULL, 'Multicheck', 6,
        'Kompetenz; Forschungsergebnis; Neue Technologien; Interesse; Informatik; Mathematik; Naturwissenschaften; Berufsbildung; Technik; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Projekt Schulinformatik AR.', 2013, 'St. Gallen', 'BSG Unternehmensberatung', 52,
        'Medienerziehung; Bericht; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Stärkung von Naturwissenschaft und Technik an den Zürcher Mittelschulen.', 2013, 'Zürich',
        'Bildungsrat', 5,
        'Bildung; Sekundarstufe II; Gymnasium; Maturitätsschule; Stundentafel; Informatik; Mathematik; Naturwissenschaften; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Winter, Ekkehard (Red.)', 'Zehn Thesen und Forderungen zur MINT-Lehramtsausbildung.', 2013, 'München',
        'Utz', 19,
        'Lehrer; Lehramtsstudiengang; Lehramtsstudium; Informatik; Mathematik; Naturwissenschaften; Beruf; Berufswahl; Studienfach; Empfehlung; Technik; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Astor, Michael; Gerres, Sebastian; Münch, Claudia; Offermann, Ruth; Pfirrmann, Oliver; Riesenberg, Daniel; Schindler, Eva; Schulze, Katrin; Seefeldt, Friedrich; Thamling, Nils',
        NULL, 'Zukunft kommt von Können.', 2013, 'Basel', NULL, 149,
        'Zukunftsperspektive; Nachwachsender Rohstoff; Wohnungsbau; Ernährung; Gesundheitswesen; Informatik; Mathematik; Lebensmitteltechnologie; Naturwissenschaften; Technologische Entwicklung; Regenerative Energie; Entwicklungshilfe; Internationale Zusammenarbeit; Soziale Verantwortung; Energiesparen; Umweltverträglichkeit; Verkehrswesen; Nachhaltige Entwicklung; Nahrungs- und Genussmittelgewerbe; Patent; Straßenbau; Transportgewerbe; Denkmalschutz; Berufsbildung; Beruf; Nachwuchs; Baugewerbe; Elektriker; Fachkraft; Fahrzeugbau; Handwerk; Umweltberuf; Infrastruktur; Innovation; Innovationsfähigkeit; Technik; Wissensmanagement');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Jungwirth, Ingrid; Grigoleit, Grit; Wolffram, Andrea; Bouffier, Anna', NULL,
        'Arbeitsmarktintegration hochqualifizierter Migrantinnen.', 2012, 'Bonn u.a.', NULL, 47,
        'Informatik; Mathematik; Naturwissenschaften; Technologie; Einwanderung; Familiennachzug; Migrationshintergrund; Wanderung; Arbeitskräftemangel; Arbeitsmarkt; Beruf; Berufsverlauf; Berufswechsel; Erwerbsbeteiligung; Qualifikationsniveau; Fachkraft; Geschlechtsspezifik; Technik; Hochschulabsolvent; Ausländerin; Aussiedler; Europäer; Hoch Qualifizierter; Migrant; Wissenschaftler; Herkunftsland; Kasachstan; Kirgistan; Moldau (Republik); Rumänien; Russland; Ukraine');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Der Arbeitsmarkt für Akademikerinnen und Akademiker in Deutschland - Naturwissenschaften/ Informatik.', 2012,
        'Nürnberg', NULL, 100,
        'Beschäftigungsentwicklung; Arbeitskräftebedarf; Arbeitslosenquote; Arbeitslosigkeit; Arbeitsmarktentwicklung; Berufseinmündung; Berufsgruppe; Akademiker; Bachelor-Studiengang; Geschlechterverteilung; Personalbedarf; Regionaler Vergleich; Hochschulabsolvent; Sozialversicherungspflichtiger Arbeitnehmer; Ostdeutschland; Westdeutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Brehm, Thorsten; Eggert, Kerstin; Oberlander, Willi', NULL, 'Die Lage der Freien Berufe.', 2012, 'Nürnberg',
        NULL, 164,
        'Kultur; Demografischer Faktor; Frau; Lehrer; Informatik; Mathematik; Naturwissenschaften; Alterssicherung; Altersstruktur; Migrationshintergrund; Soziale Sicherung; Wirtschaftslage; Einkommen; Erwerbstätigkeit; Qualitätsmanagement; Wirtschaftssektor; Arbeitsloser; Arbeitssituation; Beruf; Berufliche Selbstständigkeit; Berufsstruktur; Erwerbstätiger; Nachwuchs; Arbeitszeit; Architekt; Freie Berufe; Gesundheitsberuf; Ingenieur; Journalistischer Beruf; Kulturberuf; Künstlerischer Beruf; Naturwissenschaftlicher Beruf; Rechtsanwalt; Sozialer Beruf; Steuerberater; Wirtschaftsprüfer; Technik; Europäische Union; Selbstständiger; Ostdeutschland; Westdeutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Die MINT-Klasse. Faszination Naturwissenschaften.', 2012, 'Bern', 'Gymnasium Köniz-Lerbermatt', 12,
        'Modellschule; Maturität; Modellversuch; Informatik; Mathematik; Naturwissenschaften; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Hachmeister, Cort-Denis', NULL, 'Einsam an der Spitze.', 2012, 'Gütersloh', NULL, 24,
        'Frau; Arbeitskraft; Frauenerwerbstätigkeit; Naturwissenschaftler; Wissenschaft; Hochschullehrer; Frauenstudien; Wissenschaftlicher Nachwuchs; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Lindauer, Thomas; Riss, Maria; Schmellentin, Claudia', NULL,
        'Empfehlungen für die sprachbewusste Gestaltung von Lehrmitteln: Mai 2012, im Auftrag des Regierungsauschusses des Bildungsraums.',
        2012, NULL, 'PH FHNW', 9,
        'Unterrichtsmethode; Lehrmittel; Informatik; Mathematik; Naturwissenschaften; Entwicklung; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Ehlert, Albrecht', NULL,
        'Empirische Studie: Unterschiede im Lernerfolg und Unterschiede im subjektiven Erleben des Unterrichts von Schülerinnen und Schülern im Informatik-Anfangsunterricht (11. Klasse Berufliches Gymnasium) in Abhängigkeit von der zeitlichen Reihenfolge der Themen (OOP-First und OOP-Later).',
        2012, 'Berlin', 'Freie Universität Berlin', 389,
        'Empirische Untersuchung; Lernerfolg; Informatikunterricht; Objektorientierte Programmierung; Dissertation');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Hennings, Mareike', NULL, 'Fortbildungsbedarf von Informatik-Absolventinnen und -Absolventen.', 2012,
        'Gütersloh', NULL, 14, 'Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Stabler, Jochen', NULL, 'Frauen und Männer in MINT-Berufen in Rheinland-Pfalz.', 2012, 'Nürnberg', NULL, 53,
        'Frau; Informatik; Mathematik; Naturwissenschaften; Arbeitslosigkeit; Arbeitsmarktchance; Beruf; Berufswahl; Männerberuf; Studienwahl; Technik; Mann; Sozialversicherungspflichtiger Arbeitnehmer; Rheinland-Pfalz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Geschlechtsspezifische Berufswahl von jungen Frauen und ihre Situation im Ausbildungssystem.',
        2012, 'Berlin', 'Bundestag', 60,
        'Bildungsbeteiligung; Gender Mainstreaming; Frau; Informatik; Mathematik; Naturwissenschaften; Berufsausbildung; Beruf; Berufsberatung; Berufsorientierung; Berufswahl; Technischer Beruf; Ausbildungsquote; Ausbildungssituation; Geschlechterverteilung; Geschlechtsspezifik; Technik; Bundesagentur für Arbeit; Auszubildender; Junger Erwachsener; Weibliche Jugendliche');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Köller, Katharina', NULL,
        'Individualisierung orthographischer Normerwerbsprozesse durch digital gestützte Rechtschreibstrategien - theoretische Grundlagen und exemplarische Konzeption einer Lernsoftware.',
        2012, 'Paderborn', 'Universitätsbibliothek', 326,
        'Elektronische Medien; Medieneinsatz; Individualisierung; Lernsoftware; Rechtschreibung; Norm; Dissertation; Konzeption; Theorie');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Dörge, Christina', NULL, 'Informatische Schlüsselkompetenzen.', 2012, 'Oldenburg',
        'BIS der Universität Oldenburg', 492,
        'Kompetenz; Didaktik; Informatik; Informationstechnik; Schlüsselqualifikation; Dissertation');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Jahresbericht 2011.', 2012, 'Bern', 'VSWO', 48, 'Jahresbericht');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Konzept zur Förderung des Interesses für Naturwissenschaften und Technik: Ausgangslage, Leitlinien, Massnahmen, Konsequenzen.',
        2012, 'Vaduz', 'SA', 20,
        'Sekundarstufe II; Gymnasium; Interesse; Unterricht; Fächerwahl; Informatik; Mathematikunterricht; Naturwissenschaften; Naturwissenschaftlicher Unterricht; Ausbildung; Ausbildungswahl; Technik; Liechtenstein');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Ansmann, Moritz; Roessler, Isabel', NULL, 'Master Ranking Informatik 2012.', 2012, 'Gütersloh', NULL, 28,
        'Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Brand, Peter; Greder-Specht, Christine; Schefer, Niklaus', NULL,
        'MINT-Offensive: der emotionale und alltagsbezogene Zugang ist wichtig.', 2012, 'Bern',
        'Mittelschul- und Berufsbildungsamt des Kantons Bern', 1,
        'Maturitätsschule; Interesse; Fachdidaktik; Informatik; Mathematik; Naturwissenschaften; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Perspektive MINT.', 2012, 'Berlin [u.a.]', NULL, 77,
        'Förderung; Mathematik; Ingenieurwissenschaft; Naturwissenschaften; Berufswahl; Ingenieur; Mathematiker; Naturwissenschaftler; Wissenschaft; Technik; Wissenschaftlicher Nachwuchs');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Thomann, Lisa Caroline', NULL, 'Portfolio im Modellstudiengang Medizin der RWTH Aachen.', 2012, 'Aachen',
        'Hochschulbibliothek Rheinisch-Westfälische Technischen Hochschule Aachen', 164,
        'Fragebogen; Interview; Elektronische Medien; Medizin; Hochschullehre; Dissertation; Pflichtenheft; Portfolio; Nutzer; Student');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Federkeil, Gero', NULL, 'Praxis-Check 2012.', 2012, 'Gütersloh', NULL, 13,
        'Beschäftigungsfähigkeit; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Christine, Bescherer; Christian, Spannagel; Zimmermann, Marc; Hoffkamp, Andrea; Moll, Gabriele', NULL,
        'Semiautomatische Analyse individueller Lernprozesse in der Mathematik.', 2012, 'Ludwigsburg',
        'Pädagogische Hochschule Ludwigsburg', 42,
        'Lehrkompetenz; Lernkompetenz; Computerunterstützter Unterricht; Mathematikunterricht; Dozent; Hochschuldidaktik; Hochschule; Fortbildung; Projektbericht');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Hofmann-Lun, Irene; Rother, Jessica', NULL, 'Sind MINT-Berufe zukunftsträchtig auch für Hauptschülerinnen?',
        2012, 'München u.a.', 'Dt. Jugendinst.', 68,
        'Expertenbefragung; Interview; Frau; Hauptschule; Weiterführende Schule; Lehrer; Schüler; Schülerin; Außerschulische Tätigkeit; Projekt; Informatik; Mathematik; Naturwissenschaften; Berufsorientierung; Berufswahl; Ausbildungsberuf; Pädagoge; Einflussfaktor; Konzeption; Technik; Forschungsprojekt; Bayern; Deutschland; Hamburg; München');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kernen, Nora; Riss, Maria', 'Lindauer, Thomas (Mitarb.); Schmellentin, Claudia (Mitarb.)',
        'Textschwierigkeiten in Lehrmitteln für den naturwissenschaftlichen Unterricht in der Sekundarstufe I.', 2012,
        NULL, 'PH FHNW', 39,
        'Unterrichtsmethode; Lehrmittel; Informatik; Mathematik; Naturwissenschaften; Entwicklung; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Leisering, Benedikt; Rolff, Katharina', NULL, 'Was bindet junge Akademiker an Arbeitsplätze in der Region?',
        2012, 'Gelsenkirchen', 'IAT', 14,
        'Zukunftsperspektive; Image; Region; Standort; Informatik; Mathematik; Naturwissenschaften; Arbeitsort; Arbeitsplatz; Arbeitsplatzanalyse; Arbeitsplatzwahl; Arbeitsqualität; Beruf; Regionaler Arbeitsmarkt; Regionaler Faktor; Standortfaktor; Technik; Hochschulabsolvent; Nordrhein-Westfalen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, '4. Bilanz Chancengleichheit.', 2011, 'Berlin', NULL, 88,
        'Bildungsbeteiligung; Familie; Kinderbetreuung; Informatik; Mathematik; Naturwissenschaften; Einkommensunterschied; Gleichstellungspolitik; Erwerbsquote; Erwerbstätigkeit; Personalpolitik; Privatwirtschaft; Berufsausbildung; Beruf; Erwerbsbeteiligung; Geringfügige Beschäftigung; Teilzeitbeschäftigung; Berufswahl; Unternehmer; Hochschulbildung; Habilitation; Promotion; Studienabschluss; Weiterbildung; Best-Practice-Modell; Technik; Führungskraft');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Wüllerich, Judith; Beckmann, Ralf', NULL, 'Aktuelle Fachkräfteengpässe.', 2011, 'Nürnberg', NULL, 14,
        'Bundesland; Branche; Arbeitskräftemangel; Berufsgruppe; Mangelberuf; Fachkraft; Gesundheitsberuf; Ingenieur; Pflegerischer Beruf; Technischer Beruf; Regionaler Vergleich; Sektorale Verteilung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Puhlmann, Angelika; Gutschow, Katrin; Rieck, Andrea',
        'Klab, Jessica (Mitarb.); Reiß, Nina (Mitarb.); Terp, Christina (Mitarb.)',
        'Berufsorientierung junger Frauen im Wandel.', 2011, 'Bonn', NULL, 33,
        'Geschlechtsspezifische Sozialisation; Frau; Ausbildungsentscheidung; Berufsorientierung; Berufswahl; Geschlechtsspezifik; Auszubildender');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Konegen-Grenier, Christiane; Lotz, Sebastian; Plünnecke, Axel', NULL,
        'Bildungsgerechtigkeit in Deutschland.', 2011, 'Köln', 'Institut der deutschen Wirtschaft Köln Medien GmbH',
        106, 'Bildungschance; Bildungspolitik; Bildungsökonomie; Soziale Gerechtigkeit; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Burkhart, Simone; Heublein, Ulrich; Wank, Johanna', NULL, 'Bildungsinländer 2011.', 2011, 'Bonn', NULL, 57,
        'Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Knobeldorf, Maria', NULL, 'Biographische Lern- und Bildungsprozesse im Handlungskontext der Computernutzung.',
        2011, 'Berlin', 'Freie Universität Berlin', 351,
        'Qualitative Forschung; Computer; Informatikunterricht; Dissertation; Biografie; Theorie');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Der Arbeitsmarkt in Deutschland.', 2011, 'Nürnberg', NULL, 14,
        'Frau; Informatik; Mathematik; Naturwissenschaften; Beschäftigungsentwicklung; Arbeitskräftebedarf; Arbeitskräftemangel; Arbeitslosenquote; Arbeitslosigkeit; Arbeitsmarktentwicklung; Arbeitsmarktstatistik; Arbeitsplatzangebot; Beruf; Fachkraft; Ingenieur; Technik; Hochschulabsolvent; Sozialversicherungspflichtiger Arbeitnehmer; Studienanfänger');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Fiedler, Julian', NULL,
        'Einsatz eines Webquests zur Förderung des selbstorganisierten Lernens im Rahmen der Lehrplaneinheit ''Informatik und Gesellschaft'' in einer Eingangsklasse des Wirtschaftsgymnasiums.',
        2011, 'München', 'GRIN-Verl.', 117, 'Wirtschaftsgymnasium; Selbstgesteuertes Lernen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Roessler, Isabel', NULL, 'Erweiterung des CHE-HochschulRankings.', 2011, 'Gütersloh', NULL, 23,
        'Informatik; Studium; Studienverlauf; Studiensituation; Master-Studiengang; Studentische Bewertung; Forschungsbezug; Praxisbezug; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Fakten und Position der Bundesregierung zum sogenannten Fachkräftemangel.', 2011, NULL, NULL, 48,
        'Erhebung; Informatik; Mathematik; Naturwissenschaften; Migrationshintergrund; Mismatch; Niedriglohn; Berufsausbildung; Arbeitskräftebedarf; Arbeitskräftemangel; Arbeitslosigkeit; Arbeitsmarktforschung; Arbeitsmarkttheorie; Arbeitsplatzangebot; Atypische Beschäftigung; Beruf; Stellenbesetzung; Fachkraft; Bedarf; Personalbedarf; Regionalverteilung; Rekrutierung; Sektorale Verteilung; Stille Reserve; Technik; Institut für Arbeitsmarkt- und Berufsforschung; Arbeitnehmer; Ausländer; Hoch Qualifizierter; Student');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Folgerungen für die Aus- und Weiterbildung von Lehrpersonen aus den Leitlinien für den Unterricht in Naturwissenschaften und Technik auf der Volksschulstufe.',
        2011, 'Zürich', 'Bildungsdirektion', 6,
        'Bildung; Lehrerbildung; Lehrerfortbildung; Didaktik; Fachdidaktik; Informatik; Mathematik; Naturwissenschaften; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Folgerungen für Lehr- und Lernmittel aus den Leitlinien für den Unterricht in Naturwissenschaften und Technik auf der Volksschulstufe.',
        2011, 'Zürich', 'Bildungsdirektion', 5,
        'Bildung; Lehrmittel; Informatik; Mathematik; Naturwissenschaften; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Brück-Klingberg, Andrea; Althoff, Jörg', NULL, 'Frauen in MINT-Berufen in Niedersachsen.', 2011, 'Nürnberg',
        NULL, 51,
        'Frau; Informatik; Mathematik; Naturwissenschaften; Arbeitslosigkeit; Arbeitsmarktchance; Beruf; Berufswahl; Männerberuf; Studienwahl; Technik; Sozialversicherungspflichtiger Arbeitnehmer; Niedersachsen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Brück-Klingberg, Andrea; Althoff, Jörg', NULL, 'Frauen in MINT-Berufen in Bremen.', 2011, 'Nürnberg', NULL, 49,
        'Frau; Informatik; Mathematik; Naturwissenschaften; Arbeitslosigkeit; Arbeitsmarktchance; Beruf; Berufswahl; Männerberuf; Studienwahl; Technik; Sozialversicherungspflichtiger Arbeitnehmer; Bremen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Garloff, Alfred; Machnig, Jan; Schaade, Peter', NULL, 'Große Lohnunterschiede zwischen Frauen und Männern.',
        2011, 'Nürnberg', NULL, 46,
        'Frau; Schulabschluss; Informatik; Mathematik; Naturwissenschaften; Beschäftigungsentwicklung; Arbeitslosigkeit; Arbeitsmarktchance; Arbeitsmarktpolitik; Atypische Beschäftigung; Beruf; Erwerbsbeteiligung; Teilzeitbeschäftigung; Ausbildungsplatzangebot; Ausbildungsberuf; Geschlechterverteilung; Technik; Führungskraft; Sozialversicherungspflichtiger Arbeitnehmer; Hessen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Giesen, Birgit (Hrsg.); Zimmermann, Stefanie (Red.)', 'JobTrends Deutschland 2011.', 2011, 'Köln', NULL,
        58,
        'Informatik; Ingenieurwissenschaft; Naturwissenschaften; Lohnhöhe; Personalauswahl; Unternehmen; Arbeitsmarktchance; Berufseinmündung; Qualifikationsanforderung; Schlüsselqualifikation; Stellenangebot; Bewerbung; Informatiker; Ingenieur; Jurist; Naturwissenschaftler; Rechtswissenschaft; Wirtschaftswissenschaft; Master-Studiengang; Bachelor-Studiengang; Auswahlverfahren; Personalbedarf; Internet; Hochschulabsolvent; Berufsanfänger; Wirtschaftswissenschaftler');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Sode, Madlen; Tolciu, Andreia', NULL, 'Mehr Studienanfänger - mehr Studienabbrecher?', 2011, 'Hamburg', NULL,
        30,
        'Informatik; Mathematik; Naturwissenschaften; Beruf; Berufliche Integration; Fachhochschule; Studium; Universität; Studiensituation; Studienfach; Abbruch; Technik; Ursache; Verbleib; Hochschulabsolvent; Studienanfänger');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Anger, Christina; Erdmann, Vera; Plünnecke, Axel', NULL, 'MINT-Trendreport 2011.', 2011, 'Köln', NULL, 50,
        'Frau; Bildungspolitik; Abbruchquote; Schulbildung; Informatik; Mathematik; Naturwissenschaften; Lohnentwicklung; Wechsel; Beschäftigungsentwicklung; Arbeitskräftemangel; Arbeitsmarktentwicklung; Beruf; Nachwuchs; Berufslenkung; Fachkraft; Hochschulbildung; Studium; Studentenzahl; Studienwahl; Studienfach; Rentenalter; Bedarf; Rekrutierung; Technik; Hochschulabsolvent; Ausländer; Berufsnachwuchs; Student; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Hetze, Pascal', NULL, 'Nachhaltige Hochschulstrategien für mehr MINT-Absolventen.', 2011, 'Essen',
        'Stifterverband', 27,
        'Bildung; Motivationsförderung; Bildungsförderung; Schüler; Schulbildung; Programm; Informatik; Mathematik; Mathematikunterricht; Ingenieurwissenschaft; Naturwissenschaften; Demografischer Wandel; Arbeitskräftebedarf; Arbeitskräftemangel; Arbeitsmotivation; Beruf; Berufsinteresse; Berufslenkung; Fachkraft; Ingenieur; Naturwissenschaftlicher Beruf; Technischer Beruf; Hochschulbildung; Studium; Studentenzahl; Abbruch; Technik; Hochschulabsolvent; Studienanfänger');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Vierte Bilanz Chancengleichheit - Erfolgreiche Initiativen unterstützen - Potenziale aufzeigen.',
        2011, 'Berlin', NULL, 87,
        'Bildungsbeteiligung; Chancengleichheit; Familie; Frau; Kinderbetreuung; Indikator; Informatik; Mathematik; Naturwissenschaften; Einkommensunterschied; Gleichstellungspolitik; Erwerbsquote; Erwerbstätigkeit; Personalpolitik; Privatwirtschaft; Berufsausbildung; Beruf; Erwerbsbeteiligung; Erwerbstätiger; Geringfügige Beschäftigung; Teilzeitbeschäftigung; Berufswahl; Unternehmer; Hochschulbildung; Habilitation; Promotion; Studienabschluss; Weiterbildung; Best-Practice-Modell; Technik; Führungskraft');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Nagl, Manfred; Rüssmann, Kirsten', NULL, 'Zufriedenheit mit der Ingenieurpromotion.', 2011, 'Aachen', NULL, 21,
        'Aachen; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL,
        'Stertz, Andrea (Red.); Brengmann-Domogalla, Hedwig (Red.); Goeser, Jochen (Red.); Isenmann, Martin (Red.)',
        'AusbildungPlus in Zahlen.', 2010, 'Bonn', NULL, 56,
        'Bildungsstatistik; Prüfung; Datenbank; Informatik; Mathematik; Naturwissenschaften; Berufsbildung; Beruf; Berufsstatistik; Zusatzqualifikation; Ausbildungsinhalt; Ausbildungsplatzangebot; Ausbildungszeit; Betriebliche Berufsausbildung; Studentenzahl; Studienplatzangebot; Dualer Studiengang; Studienabschluss; Weiterbildungsangebot; Quantitative Angaben; Quote; Regionalverteilung; Technik; Zertifizierung; Auszubildender');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Bargel, Tino', NULL, 'Barrieren und Benachteiligungen für Bildungsaufsteiger.', 2010, 'Konstanz', NULL, 3,
        'Bildungschance; Soziale Ungleichheit; Soziale Herkunft; Sozioökonomischer Faktor; Hochschulbildung; Studium; Hochschulzugang; Benachteiligung; Student; Studentin');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Becker, Carsten; Grebe, Tim; Bleikertz, Torben', NULL,
        'Berufliche Integration von Studienabbrechern vor dem Hintergrund des Fachkräftebedarfs in Deutschland.', 2010,
        'Berlin', NULL, 98,
        'Soziale Situation; Informatik; Mathematik; Naturwissenschaften; Einkommenshöhe; Kleine und mittlere Unternehmen; Personalauswahl; Arbeitskräftebedarf; Beruf; Berufliche Integration; Beruflicher Verbleib; Berufseinmündung; Berufsverlauf; Bildungsadäquate Beschäftigung; Fachkraft; Studium; Studiendauer; Studienmotivation; Abbruch; Personalbedarf; Technik; Ursache; Arbeitgeber');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Budde, Jürgen', NULL,
        'Bildungs(miss)erfolge von Jungen und Berufswahlverhalten bei Jungen / männlichen Jugendlichen.', 2010,
        'Berlin', 'BMBF', 62,
        'Bildungsmotivation; Fremdeinschätzung; Gewalt; Selbstbild; Vorbild; Bildungsverlauf; Rollenverhalten; Junge; Geschlechterrolle; Medienkonsum; Schullaufbahn; Grundschule; Sekundarbereich; Lehrer; Schulbildung; Schulleistung; Monoedukation; Unterrichtsfach; Übergang Schule - Beruf; Literatur; Fremdsprachenkenntnisse; Informatik; Mathematik; Naturwissenschaften; Naturwissenschaftliche Bildung; Ausbildung; Beruf; Berufseinmündung; Übergang Ausbildung - Beruf; Berufswahl; Berufswunsch; Handwerk; Männerberuf; Technischer Beruf; Gewaltbereitschaft; Ausländer; Jugendlicher; Männlicher Jugendlicher');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Erdmann, Vera; Plünnecke, Axel; Riesen, Ilona; Stettes, Oliver', NULL, 'Bildungsmonitor 2010.', 2010, 'Köln',
        NULL, 219,
        'Bildung; Bildungschance; Bildungsdefizit; Bildungsniveau; Chancengleichheit; Bildungssystem; Benachteiligtenförderung; Forschungsaufwand; Bildungsertrag; Kinderbetreuung; Schule; Schülerzahl; Schulische Integration; Lernbehinderung; Lernschwierigkeit; Informatik; Lesen; Mathematik; Naturwissenschaften; Bundesland; Internationalisierung; Sozioökonomischer Faktor; Ausgaben; Benchmarking; Investition; Öffentliche Ausgaben; Ökonomische Determinanten; Wirtschaftswachstum; Berufsbildung; Arbeitsmarktchance; Beruf; Ausbildungszeit; Berufsvorbereitung; Studiendauer; Akademisierung; Kompetenzentwicklung; Qualität; Quote; Regionaler Vergleich; Technik; Hochschulabsolvent');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Cornetz, Wolfgang; Hell, Stefan; Kalmbach, Peter; Schäfer, Holger', NULL,
        'Chancen und Risiken des demografischen und strukturellen Wandels im Saarland.', 2010, 'Saarbrücken',
        'Universaar', 120,
        'Bildung; Bildungssystem; Stipendium; Durchlässigkeit; Informatik; Mathematik; Naturwissenschaften; Demografischer Wandel; Dienstleistungsgesellschaft; Entindustrialisierung; Humankapital; Finanzierung; Strukturwandel; Unternehmensgründung; Wirtschaft; Wirtschaftspolitik; Arbeitskräfteangebot; Arbeitskräftebedarf; Beruf; Erwerbsbeteiligung; Qualifikationsstruktur; Hochschulbildung; Studium; Bologna-Prozess; Hochschulpolitik; Hochschule; Abbruch; Abwanderung; Prävention; Technik; Saarland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Gehrig, Matthias; Gardiol, Lucien; Schaerrer, Markus', NULL, 'Der MINT-Fachkräftemangel in der Schweiz.', 2010,
        'Bern', 'SBF', 96,
        'Bildung; Fächerwahl; Informatik; Naturwissenschaften; Wirtschaftsentwicklung; Ausbildung; Berufliche Qualifikation; Berufsanforderung; Ausbildungswahl; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Fachkräftemangel bekämpfen, Wettbewerbsfähigkeit sichern.', 2010, 'Berlin', NULL, 48,
        'Familie; Frau; Bildungspolitik; Kinderbetreuung; Aktivierung; Informatik; Mathematik; Naturwissenschaften; Altersversorgung; Demografischer Wandel; Einwanderung; Einwanderungspolitik; Personalbeschaffung; Berufsbildung; Arbeitskräftemangel; Arbeitsloser; Arbeitsmarktpolitik; Beruf; Mangelberuf; Ausbildungsförderung; Ausbildungseignung; Berufsorientierung; Elektroberuf; Fachkraft; Metallberuf; Pflegerischer Beruf; Arbeitsanreiz; Lebenslanges Lernen; Altersgrenze; Arbeitsorganisation; Technik; Arbeitnehmer; Ausbildungsplatzbewerber; Ausländer; Älterer Arbeitnehmer; Behinderter');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Gruttmann, Susanne', NULL, 'Formatives E-Assessment in der Hochschullehre.', 2010, 'Münster',
        'Verl.-Haus Monsenstein und Vannerdat', 263,
        'Prüfung; Übung; Informatik; Computerunterstütztes Verfahren; Studium; Dissertation');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Dietrich, Ingrid; Fritzsche, Birgit', NULL, 'Frauen und Männer am Arbeitsmarkt in Sachsen-Anhalt.', 2010,
        'Nürnberg', NULL, 51,
        'Frau; Arbeitslosigkeit; Arbeitsmarktchance; Arbeitsmarktpolitik; Berufsstruktur; Erwerbsbeteiligung; Qualifikationsniveau; Teilzeitbeschäftigung; Informationstechnischer Beruf; Naturwissenschaftlicher Beruf; Technischer Beruf; Mann; Sozialversicherungspflichtiger Arbeitnehmer; Sachsen-Anhalt');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Bösch, Nicole', NULL, 'Frauen und Männer am Ausbildungs- und Arbeitsmarkt in Baden-Württemberg.', 2010,
        'Nürnberg', NULL, 65,
        'Frau; Informatik; Mathematik; Naturwissenschaften; Arbeitskräftebedarf; Arbeitslosigkeit; Arbeitsmarktchance; Beruf; Erwerbsbeteiligung; Teilzeitbeschäftigung; Ausbildungsplatzangebot; Berufswunsch; Geschlechterverteilung; Technik; Allein erziehender Elternteil; Mann; Migrant; Baden-Württemberg');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Barthel, Hannelore', NULL, 'Informatikunterricht.', 2010, NULL, NULL, 191,
        'Erwartung; Schüler; Schülerin; Schülerbefragung; Didaktik; Informatik; Informatikunterricht; Dissertation');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Ihsen, Susanne; Hantschel, Victoria; Hackl-Herrwerth, Andrea; Wienefoet, Verena; Baldin, Dominik', NULL,
        'Ingenieurwissenschaften: Attraktive Studiengänge und Berufe auch für Menschen mit Migrationshintergrund?',
        2010, 'München', NULL, 20,
        'Informatik; Ingenieurwissenschaft; Arbeitskräftemangel; Beruf; Fachkraft; Ingenieur; Studium; Studiengang; Hochschule; Geschlechtsspezifik; Migrant');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Zorn, Isabel', NULL, 'Konstruktionstätigkeit mit Digitalen Medien.', 2010, 'Bremen', NULL, 490,
        'Bildung; Bildungstheorie; Allgemeinbildung; Empirische Untersuchung; Interview; Kreativität; Digitale Medien; Elektronische Medien; Medien; Medienarbeit; Mediendidaktik; Medienerziehung; Medienkompetenz; Medienpädagogik; Medienverhalten; Interaktion; Lernprozess; Computer; Computergrafik; Software; Informatik; Softwaretechnologie; Ästhetik; Design; Informationstechnik; Gestaltung; Hochschulschrift; Lebenswelt; Modell; Technik; Erwachsener; Jugendlicher; Informationstechnologie; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Mangel an MINT-Fachkräften in der Schweiz.', 2010, 'Bern', 'Bundeskanzlei', 50,
        'Informatik; Mathematik; Naturwissenschaften; Ausbildung; Laufbahnberatung; Berufsberatung; Studienberatung; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Massnahmen zur Förderung von Naturwissenschaften und Technik in der Allgemeinbildung im Kanton Zürich.', 2010,
        'Zürich', 'Bildungsrat', 7,
        'Bildung; Allgemeinbildung; Informatik; Mathematik; Naturwissenschaften; Offizieller Text; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Heiken, Andreas', NULL, 'Nachhaltiger Ansatz für die Entwicklung einer Geoinformations-Teachware.', 2010, NULL,
        NULL, 178,
        'Lernprogramm; Geographieunterricht; Geoinformatik; Dissertation; E-Learning; Geoinformationssystem');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Ebner, Christian; Nikolai, Rita', NULL, 'Schlechtes Zeugnis für Deutschland.', 2010, 'Berlin', NULL, 8,
        'Frau; Bildungspolitik; Vorschulerziehung; Schule; Sekundarstufe II; Schulbildung; Informatik; Mathematik; Naturwissenschaften; Politik; Benchmarking; Berufsbildung; Beruf; Hochschulbildung; Weiterbildung; Lebenslanges Lernen; Internationaler Vergleich; Abbruch; Technik; Umsetzung; Europäische Union; Deutschland; Europa');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Vorschläge zur Förderung von Naturwissenschaft und Technik in der Allgemeinbildung im Kanton Zürich.', 2010,
        'Zürich', 'Bildungsdirektion', 20,
        'Bildung; Lehrerbildung; Lehrerfortbildung; Maturität; Lehrmittel; Informatik; Mathematik; Naturwissenschaften; Steuerung; Technik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Hohberg, Maike; Hamann, Silke', NULL,
        'Ausbildung und Beschäftigung von Frauen in MINT-Berufen in Baden-Württemberg.', 2009, 'Nürnberg', NULL, 35,
        'Frau; Mathematik; Ingenieurwissenschaft; Naturwissenschaften; Arbeitslosigkeit; Arbeitsmarktchance; Berufswahl; Männerberuf; Betriebliche Berufsausbildung; Studienwahl; Geschlechterverteilung; Technik; Baden-Württemberg');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL,
        'Stertz, Andrea (Red.); Brengmann-Domogalla, Hedwig (Red.); Isenmann, Martin (Red.); König, Maik (Red.); Kupfer, Franziska (Red.)',
        'AusbildungPlus in Zahlen.', 2009, 'Bonn', NULL, 27,
        'Datenbank; Zusatzqualifikation; Dualer Studiengang; Quantitative Angaben');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schramm, Michael; Kerst, Christian', 'Rehn, Torsten (Mitarb.)',
        'Berufseinmündung und Erwerbstätigkeit in den Ingenieur- und Naturwissenschaften.', 2009, 'Hannover', NULL, 132,
        'Zufriedenheit; Frau; Mathematik; Ingenieurwissenschaft; Naturwissenschaften; Arbeitskräftemangel; Ausgeübter Beruf; Beruf; Berufliche Mobilität; Berufseinmündung; Berufsverlauf; Berufswechsel; Bildungsadäquate Beschäftigung; Erwerbstätiger; Qualifikationsanforderung; Qualifikationspotenzial; Fachkraft; Ingenieur; Naturwissenschaftler; Naturwissenschaftlicher Beruf; Technischer Beruf; Erwerbsunterbrechung; Dequalifizierung; Forschungsbericht; Hochschulabsolvent');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Leuprecht, Eva; Putz, Ingrid; Paul, Verena; Kasper, Ruth; Steiner, Karin; Wittinger, Daniela; Kittel, Carmen',
        NULL,
        'Berufseinstieg, Joberfahrungen und Beschäftigungschancen von AbsolventInnen technisch-naturwissenschaftlicher FH-Studiengänge.',
        2009, 'Wien', NULL, 325,
        'Zufriedenheit; Biotechnologie; Luftverkehr; Mathematik; Informationstechnik; Ingenieurwissenschaft; Naturwissenschaften; Produktionstechnik; Elektronik; Beschäftigungsentwicklung; Arbeitskräftebedarf; Arbeitsmarktchance; Berufseinmündung; Berufsverlauf; Berufswahl; Architektur; Bautechnik; Elektrotechnik; Fahrzeugbau; Ingenieur; Maschinenbauingenieur; Naturwissenschaftler; Fachhochschule; Studienwahl; Abschlussbericht; Technik; Hochschulabsolvent; Österreich');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Plünnecke, Axel; Riesen, Ilona; Stettes, Oliver', NULL, 'Bildungsmonitor 2009.', 2009, 'Köln', NULL, 229,
        'Bildung; Bildungschance; Bildungsdefizit; Bildungsniveau; Chancengleichheit; Bildungssystem; Benachteiligtenförderung; Forschungsaufwand; Bildungsertrag; Kinderbetreuung; Schule; Schülerzahl; Schulische Integration; Lernbehinderung; Lernschwierigkeit; Informatik; Lesen; Mathematik; Naturwissenschaften; Bundesland; Internationalisierung; Sozioökonomischer Faktor; Ausgaben; Benchmarking; Investition; Öffentliche Ausgaben; Ökonomische Determinanten; Wirtschaftswachstum; Berufsbildung; Arbeitsmarktchance; Beruf; Ausbildungszeit; Berufsvorbereitung; Studiendauer; Akademisierung; Effizienz; Forschungsbericht; Kompetenzentwicklung; Qualität; Regionaler Vergleich; Hochschulabsolvent');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Hollricher, Karin (Bearb.)', 'Chancengerechtigkeit in Bildung und Forschung.', 2009, 'Bonn u.a.', 'BMBF',
        22,
        'Forschung; Bildungsbeteiligung; Chancengleichheit; Frau; Förderung; Programm; Geschlecht; Informatik; Mathematik; Naturwissenschaften; Ausbildungsniveau; Wissenschaft; Technik; Führungskraft; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schrackmann, Iwan', NULL, 'Computer an der Volksschule des Kantons Schwyz.', 2009, 'Schwyz',
        'Amt für Volksschulen und Sport', 34, 'Software; Hardware; Informatik; Forschungsbericht; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Berghoff, Sonja; Federkeil, Gero; Giebisch, Petra; Hachmeister, Cort-Denis; Hennings, Mareike; Roessler, Isabel; Ziegele, Frank',
        NULL, 'Das CHE-Forschungsranking deutscher Universitäten 2009.', 2009, 'Gütersloh', NULL, 258,
        'Ranking; Erfindung; Ingenieurwissenschaft; Finanzierung; Hochschulforschung; Promotion; Studienbedingungen; Wissenschaftliches Arbeiten; Humanwissenschaften; Wirtschaftswissenschaft; Hochschule; Publikation; Reputation; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Deutsch-Chilenische Forschungszusammenarbeit: Tradition und Zukunft.', 2009, 'Bonn', NULL, 137,
        'Forschung; Bildungsreform; Biologie; Informatik; Entwicklungsland; Internationale Zusammenarbeit; Wissenschaft; Rechtswissenschaft; Hochschule; Sozialwirtschaft; Konferenzschrift; Kooperation; Andenraum; Brasilien; Chile; Deutschland; Lateinamerika; Südamerika');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Dietrich, Ingrid; Fritzsche, Birgit', NULL, 'Die Arbeitsmarktsituation von Frauen in Thüringen.', 2009,
        'Nürnberg', NULL, 41,
        'Frau; Mathematik; Ingenieurwissenschaft; Naturwissenschaften; Arbeitslosigkeit; Arbeitsmarktchance; Arbeitsmarktpolitik; Berufsstruktur; Erwerbsbeteiligung; Qualifikationsniveau; Teilzeitbeschäftigung; Berufswahl; Geschlechterverteilung; Technik; Sozialversicherungspflichtiger Arbeitnehmer; Thüringen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Die Entwicklung der MINT-Fachkräftelücke in Deutschland.', 2009, 'Berlin', NULL, 12,
        'Bildung; Bildungspolitik; Informatik; Mathematik; Elektrotechnische Industrie; Personalpolitik; Arbeitskräftemangel; Arbeitsmarktentwicklung; Arbeitsplatzangebot; Beruf; Fachkraft; Ingenieur; Metallindustrie; Hochschulpolitik; Berufsbegleitendes Studium; Dualer Studiengang; Arbeitspapier; Konjunkturabhängigkeit; Personalbedarf; Prognose; Technik; Berufsnachwuchs');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Fahrner, Ulrich', NULL, 'Die explorative Datenanalyse als Lern- und Erkenntniswerkzeug.', 2009, NULL, NULL,
        184,
        'Explorative Datenanalyse; Lernpsychologie; Psychologie; Mediendidaktik; Konstruktivismus; Dissertation; Statistik');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Solga, Heike; Pfahl, Lisa', NULL, 'Doing Gender im technisch-naturwissenschaftlichen Bereich.', 2009, 'Berlin',
        NULL, 57,
        'Bildung; Frau; Geschlechterbeziehung; Frauenförderung; Kindergarten; Schule; Naturwissenschaften; Hochschulbildung; Studium; Studienwahl; Studienbedingungen; Studienverlauf; Studiensituation; Abbruch; Geschlechterverteilung; Technik; Mann; Student; Studentin; Studienanfänger; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Dietrich, Ingrid; Fritzsche, Birgit', NULL, 'Duale Ausbildung in MINT-Berufen in Sachsen-Anhalt.', 2009,
        'Nürnberg', NULL, 35,
        'Bildungserfolg; Realschule; Mittlerer Schulabschluss; Schulabschluss; Regionale Disparität; Ausbildung; Naturwissenschaftlicher Beruf; Technischer Beruf; Betriebliche Berufsausbildung; Abbruch; Geschlechterverteilung; Absolvent; Sachsen-Anhalt');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Dietrich, Ingrid; Fritzsche, Birgit', NULL, 'Duale Ausbildung in MINT-Berufen in Thüringen.', 2009, 'Nürnberg',
        NULL, 36,
        'Bildungserfolg; Regionale Disparität; Mathematik; Ausbildung; Ausbildungsplatzangebot; Ingenieur; Naturwissenschaftlicher Beruf; Betriebliche Berufsausbildung; Abbruch; Geschlechterverteilung; Absolvent; Thüringen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Stechert, Peer', NULL,
        'Fachdidaktische Diskussion von Informatiksystemen und der Kompetenzentwicklung im Informatikunterricht.', 2009,
        'Potsdam', 'Univ.-Verl.', 256,
        'Fachdidaktik; Informatik; Informatikunterricht; Dissertation; Kompetenzentwicklung; System');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Mesaros, Leila; Vanselow, Achim; Weinkopf, Claudia', NULL, 'Fachkräftemangel in KMU.', 2009, 'Bonn', NULL, 56,
        'Demografischer Wandel; Migrationshintergrund; Arbeitsbedingungen; Branche; Kleine und mittlere Unternehmen; Personalbeschaffung; Personalpolitik; Arbeitskräftemangel; Arbeitsplatz; Arbeitsplatzangebot; Berufsgruppe; Facharbeiter; Fachkraft; Gesundheitsberuf; Ingenieur; Technischer Beruf; Betriebliche Berufsausbildung; Betriebliche Weiterbildung; Altenpflege; Gutachten; Regionalverteilung; Rekrutierung; Sektorale Verteilung; Hochschulabsolvent; Arbeitnehmer; Ausländer; Älterer Arbeitnehmer');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Herdin, Gunvald; Langer, Markus F.; Stuckrad, Thimo von', NULL,
        'Fächerpräferenzen von Studienanfänger(inne)n 2006/2007 - regionale und geschlechtsspezifische Muster.', 2009,
        'Gütersloh', NULL, 39,
        'Geschlecht; Stadt; Informatik; Mathematik; Naturwissenschaften; Bundesland; Demografie; Arbeitskräfteangebot; Arbeitskräftebedarf; Fachkraft; Hochschulbildung; Hochschulpolitik; Studienwahl; Studienort; Studienortwahl; Studienfach; Technik; Studienanfänger; Deutschland; Deutschland-Östliche Länder; Deutschland-Westliche Länder');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Böhme, Stefan; Unte, Pia; Werner, Daniel', NULL, 'Frauen in MINT-Berufen in Bayern.', 2009, 'Nürnberg', NULL,
        42,
        'Frau; Informatik; Mathematik; Arbeitslosigkeit; Arbeitsmarktchance; Beruf; Berufswahl; Männerberuf; Sozialversicherungspflichtiger Arbeitnehmer; Bayern');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Konzept zur Stärkung der Naturwissenschaften am Gymnasium.', 2009, 'St. Gallen', 'Erziehungsrat',
        25,
        'Interesse; Informatik; Informatikunterricht; Mathematikunterricht; Naturwissenschaften; Naturwissenschaftlicher Unterricht; Technikunterricht; Ausbildung; Technischer Beruf; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Spohrer, Matthias', NULL,
        'Konzeption und Analyse neuer Maßnahmen in der Fort- und Weiterbildung von Informatiklehrkräften.', 2009, NULL,
        NULL, 307, 'Lehrer; Informatikunterricht; Dissertation; Fortbildung; Weiterbildung; Analyse; Konzeption');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Nachwuchsbarometer Technikwissenschaften.', 2009, 'München u.a.', NULL, 66,
        'Bildung; Stereotyp; Akzeptanz; Sozialisation; Familie; Geschlechterrolle; Schüler; Schulbildung; Unterricht; Informatik; Mathematik; Ingenieurwissenschaft; Naturwissenschaften; Arbeitskräftemangel; Beruf; Berufsprestige; Berufsberatung; Berufsvorbereitung; Berufswahl; Berufswunsch; Fachkraft; Ingenieur; Naturwissenschaftler; Naturwissenschaftlicher Beruf; Technischer Beruf; Studienwahl; Tätigkeitsfeld; Technik; Berufsnachwuchs; Student');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Uthmann, Claudia', NULL,
        'Studierendenauswahl - Erprobung und Evaluation eines multidimensionalen testdiagnostischen Verfahrens zur Studienerfolgsprognose an der Fachhochschule Heidelberg.',
        2009, NULL, NULL, 259,
        'Validität; Eignung; Eignungsdiagnostik; Intelligenztest; Persönlichkeitstest; Psychodiagnostik; Hochschulzulassung; Studierfähigkeit; Dissertation; Studienerfolg; Studienerfolgsprognose; Auswahl; Prognose; Student');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Von Agrar- bis Wirtschaftswissenschaften - Alle über einen Leisten?', 2009, 'Hannover', NULL, 155,
        'Evaluation; Forschung; Kriterium; Niedersachsen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Seger, Mario; Beuthel, Regina; Schmiede, Rudi', 'Schmiede, Rudi (Hrsg.)', 'Wege zum lifelong learning.', 2009,
        'Aachen', 'Shaker', 304,
        'Bildungsforschung; Informatik; Ausbildung; Berufliche Fortbildung; Beruflicher Aufstieg; Informationstechnischer Beruf; Weiterbildung; Erwachsenenbildung; Informationstechnologie; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Solga, Heike; Pfahl, Lisa', NULL,
        'Wer mehr Ingenieurinnen will, muss bessere Karrierechancen für Frauen in Technikberufen schaffen.', 2009,
        'Berlin', NULL, 6,
        'Chancengleichheit; Familie; Frau; Gleichstellung; Mathematik; Gleichstellungspolitik; Unternehmenskultur; Arbeitsmarktchance; Arbeitsplatzsicherheit; Beruf; Beruflicher Aufstieg; Berufsverlauf; Berufswahl; Fachkraft; Ingenieur; Männerberuf; Naturwissenschaftler; Naturwissenschaftlicher Beruf; Technischer Beruf; Studienwahl; Bedarf');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Mandl, Thomas (Hrsg.); Frommholz, Ingo (Hrsg.)', 'WIR 2009.', 2009, 'Darmstadt', 'TU, Informatik', 100,
        'Evaluationsforschung; Elektronische Bibliothek; Multimedia; Nutzerverhalten; Suchmaschine; Semantik; Mehrsprachigkeit; Datenbank; Technologie; Dokument; Konferenzschrift; Recherche; Tagungsbericht; Internet; Information Retrieval; Informationswissenschaft; Wissensmanagement; World Wide Web');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schneeberger, Arthur; Leuprecht, Eva; Kreiml, Thomas; Muralter, Doris; Kozeluh, Ulrike',
        'Egger-Subotitsch, Andrea (Hrsg.); Sturm, Rene (Hrsg.)', 'Zwischen Modernisierung und Stagnation.', 2009,
        'Wien', 'Communicatio - Kommunikations- und Publikationsgmbh', 68,
        'Informatik; Mathematik; Naturwissenschaften; Einkommenshöhe; Beschäftigungsentwicklung; Beschäftigungsform; Arbeitskräftemangel; Arbeitsmarktchance; Beruf; Berufschance; Berufseinmündung; Berufsverlauf; Fachkraft; Naturwissenschaftlicher Beruf; Psychologe; Publizist; Technischer Beruf; Übersetzer; Fachhochschule; Universität; Kulturwissenschaft; Akademiker; Technik; Hochschulabsolvent; Geisteswissenschaftler; Historiker; Sozialwissenschaftler; Österreich');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Budde, Jürgen', NULL,
        'Bildungs(miss)erfolge von Jungen und Berufswahlverhalten bei Jungen / männlichen Jugendlichen.', 2008,
        'Berlin', 'BMBF', 62,
        'Bildungsmotivation; Fremdeinschätzung; Gewalt; Selbstbild; Vorbild; Bildungsverlauf; Rollenverhalten; Junge; Geschlechterrolle; Medienkonsum; Schullaufbahn; Grundschule; Sekundarbereich; Lehrer; Schulbildung; Schulleistung; Monoedukation; Unterrichtsfach; Übergang Schule - Beruf; Literatur; Fremdsprachenkenntnisse; Informatik; Mathematik; Naturwissenschaften; Naturwissenschaftliche Bildung; Ausbildung; Beruf; Berufseinmündung; Übergang Ausbildung - Beruf; Berufswahl; Berufswunsch; Handwerk; Männerberuf; Technischer Beruf; Gewaltbereitschaft; Ausländer; Jugendlicher; Männlicher Jugendlicher');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Schubert, Sigrid (Hrsg.); Stechert, Peer (Hrsg.)',
        'Bildungskonzepte für Internetworking und eingebettete Mikrosysteme.', 2008, 'Siegen', 'Univ.-Verl. Universi',
        198,
        'Empirische Untersuchung; Feldforschung; Kommunikation; Sekundarstufe I; Sekundarstufe II; Gymnasium; Didaktik; Lernziel; Unterrichtsmodell; Fachdidaktik; Projekt; Informatik; Informatikunterricht; Informationskompetenz; Hochschule; E-Learning; Entwicklung; Kooperation; Modellierung; Internet; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kerst, Christian; Schramm, Michael', NULL,
        'Der Absolventenjahrgang 2000/2001 fünf Jahre nach dem Hochschulabschluss.', 2008, 'Hannover', 'HIS', 242,
        'Längsschnittuntersuchung; Panel; Zufriedenheit; Übergang; Erwerbstätigkeit; Arbeitsmarkt; Berufslaufbahn; Berufssituation; Studium; Hochschulabschluss; Promotion; Statistik; Studienfach; Akademiker; Weiterbildung; Absolvent; Berufsanfänger; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Zehnder, Carl August', NULL, 'Die dramatische Erosion der Informatikausbildung in der Schweiz.', 2008, NULL,
        NULL, 18,
        'Schule; Fächerwahl; Informatik; Unternehmen; Wirtschaftsentwicklung; Ausbildung; Laufbahnberatung; Ausbildungswahl; Berufsberatung; Studienberatung; Technik; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Pixner, Johann', NULL, 'Erfolgskritische Anforderungen im Hochschulstudium.', 2008, NULL, NULL, 193,
        'Arbeitspsychologie; Empirische Psychologie; Selbstbeurteilung; Psychodiagnostik; Test; Berufswahl; Hochschulbildung; Studium; Studienwahl; Dissertation; Studienerfolg; Anforderung; Online-Angebot');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schauer, Carola', NULL,
        'Größe und Ausrichtung der Disziplin Wirtschaftsinformatik an Universitäten im deutschsprachigen Raum.', 2008,
        'Essen', 'ICB', 71, 'Wirtschaftsinformatik; Hochschule; Deutscher Sprachraum; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kleimann, Bernd', NULL, 'Kapazitätseffekte von E-Learning an deutschen Hochschulen.', 2008, 'Hannover', NULL,
        96,
        'Multimedia; Lehre; Kommunikationstechnik; Studienreform; Master-Studiengang; Akademischer Grad; Bachelor-Studiengang; E-Learning; Internet; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Nachhaltige Hochschulstrategien für mehr MINT-Absolventen.', 2008, 'Essen',
        'Edition Stifterverband', 26, 'Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Zimmerhofer, Alexander', NULL,
        'Studienberatung im deutschen Hochschulsystem auf der Basis psychologischer Tests.', 2008, NULL, NULL, 245,
        'Validität; Eignung; Intelligenzmessung; Test; Akzeptanz; Interesse; Informatik; Elektrotechnik; Studium; Studieneignung; Studienwahl; Studierfähigkeit; Studienberatung; Dissertation; Studienneigung; Studienmotivation; Abbruch; Auswahlverfahren; Internet');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Krex, Larissa', NULL, 'Studienerfolgsprognose in der Bundeswehr.', 2008, 'Bonn', NULL, 267,
        'Indikator; Ausbildung; Offizier; Studium; Studienberatung; Studienerfolg; Studienverlauf; Studiengang; Hochschule; Hochschulschrift; Prognose; Selektion; Verfahren; Bundeswehr; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Mosberger, Brigitte; Salfinger, Brigitte; Kreiml, Thomas; Putz, Ingrid; Schopf, Anna', NULL,
        'Berufseinstieg, Joberfahrungen und Beschäftigungschancen von UNI-AbsolventInnen in der Privatwirtschaft.',
        2007, 'Wien', 'AMS', 220,
        'Kommunikationswissenschaft; Psychologie; Zufriedenheit; Publizistik; Biologe; Biologie; Informatik; Betriebswirtschaft; Einkommenshöhe; Privatwirtschaft; Beschäftigungsform; Arbeit; Arbeitsmarktchance; Arbeitsuche; Beruf; Berufschance; Berufseinmündung; Berufseintritt; Berufserfahrung; Berufsverlauf; Erwerbsverlauf; Architekt; Architektur; Betriebswirt; Informatiker; Psychologe; Publizist; Universität; Akademikerberuf; Studiendauer; Studienverlauf; Studienmotivation; Dauer; Abschlussbericht; Tätigkeitsfeld; Hochschulabsolvent; Wissenschaftler; Österreich');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Bildung für den Bedarf?', 2007, 'Berlin', NULL, 70, 'Konferenzbericht; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Fässler, Lukas Emanuel', NULL, 'Das 4-Schritte-Modell.', 2007, 'Zürich', 'ETH', 266,
        'Lernen; Lernprozess; Motivation; Computerunterstützter Unterricht; Informatik; Studium; Dissertation; Studienfach');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Fries, Marlene', 'Schindler, Götz (Mitarb.)',
        'Eignungsfeststellungsverfahren und Studienerfolg: Können Eignungskriterien den Studienerfolg prognostizieren?',
        2007, 'München', 'Bayerisches Staatsinst. für Hochschulforschung und Hochschulplanung', 175,
        'Eignungsprüfung; Hochschulzugang; Studienerfolg; Auswahlverfahren; Prognose');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Ermatinger, Heinz; Cueni, Margareth', 'Landwehr, Norbert (Mitarb.)',
        'Externe Evaluation des Pilotprojekts Informatik Berufsbildung i-zh im Kanton Zürich.', 2007, 'Aarau',
        'Fachhochschule Nordwestschweiz', 71, 'Evaluation; Modellversuch; Informatik; Berufsbildung; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Flubacher, Mi-Cha', NULL, 'Fächerwahl - Zusatzstudie Informatik.', 2007, 'Bern', 'CEST', 59,
        'Sekundarstufe II; Maturitätsschule; Fächerwahl; Informatik; Studium; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Komoss, Regine', NULL, 'Frauenstudiengänge.', 2007, NULL, NULL, 285,
        'Frau; Frauenanteil; Getrenntgeschlechtliche Erziehung; Monoedukation; Informatik; Studium; Studienreform; Dissertation; Studiengang; Hochschule');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Leicht-Scholten, Carmen (Hrsg.)', '''Gender and Science''.', 2007, 'Bielefeld', 'transcript', 190,
        'Forschung; Chancengleichheit; Frau; Geschlechterforschung; Handlungsorientierung; Unterricht; Geschlecht; Ingenieurwissenschaft; Naturwissenschaften; Hochschule; Geschlechtsspezifik; Konzeption; Qualitätssicherung; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Degkwitz, Andreas (Hrsg.); Schirmbacher, Peter (Hrsg.); Wefers, Sabine (Vorw.)',
        'Informationsinfastrukturen im Wandel.', 2007, 'Bad Honnef', 'Bock + Herchen', 390,
        'Forschung; Kommunikation; Informations- und Kommunikationstechnologie; Medienkompetenz; Multimedia; Förderung; Lehre; Informatik; Mathematik; Informationstechnik; Naturwissenschaften; Dienstleistung; Kundenorientierung; Informationskompetenz; Universität; Wissenschaft; Fakultät; Hochschule; E-Learning; Virtuelle Hochschule; Organisationsentwicklung; Erfahrungsbericht; Implementierung; Infrastruktur; Innovation; Integration; Modell; Organisation; Strategie; Medienzentrum; Wissenschaftliche Bibliothek; Information; Informations- und Dokumentationswissenschaft; Informationsdienst; Informationsmanagement; Augsburg; Berlin; Bielefeld; Cottbus; Deutschland; Duisburg; Essen (Stadt); Göttingen; Karlsruhe; München; Münster (Westf); Oldenburg (Oldenburg); Ulm');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Kirsten, Barbara', NULL, 'Prädiktoren einer Studienwahlentscheidung.', 2007, NULL, NULL, 269,
        'Bildungsentscheidung; Studienwahl; Dissertation; Modell');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Haffner, Yvonne; Könekamp, Bärbel; Krais, Beate', NULL, 'Arbeitswelt in Bewegung.', 2006, 'Berlin',
        'Bundesministerium für Bildung und Forschung Referat Öffentlichkeitsarbeit', 56,
        'Bildungsforschung; Chancengleichheit; Chemie; Informatik; Physik; Arbeitskultur; Ingenieurwesen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Oelkers, Jürgen', NULL, 'Expertise gymnasiale Mittelschulen.', 2006, 'Zürich',
        'Univ. Zürich, Pädagogisches Institut', 184,
        'Bildungsbeteiligung; Bildungsgang; Schulreform; Schulsystem; Gegliedertes Schulsystem; Übergang; Mittelschule; Gymnasium; Gesamtschule; Aufnahmeprüfung; Prüfung; Schulerfolg; Lehrplan; Rahmenlehrplan; Fächerkanon; Fächerwahl; Schulfach; Hochschulreife; Hochschule; Analyse; Bedarf; Empfehlung; Gutachten; Struktur; Kanton; Schweiz; Zürich');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL,
        'Metz-Göckel, Sigrid (Proj.leit.); Neef, Wolfgang (Proj.leit.); Klein, Annette (Mitarb.); Selent, Petra (Mitarb.); Kebir, Noara (Mitarb.)',
        'Fachnahe studentische Erwerbsarbeit in den Ingenieurwissenschaften und ihre Bedeutung für den Arbeitsmarkt.',
        2006, 'Berlin u.a.', NULL, 60,
        'Informatik; Ingenieurwissenschaft; Maschinenbau; Wirtschaftslage; Nebentätigkeit; Berufspraxis; Bildungsadäquate Beschäftigung; Bautechnik; Studium; Studiengebühren; Studiendauer; Zeitbudget; Abschlussbericht; Auswirkung; Nebenverdienst; Student');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Jösting, Sabine (Hrsg.); Seemann, Malwine (Hrsg.)', 'Gender und Schule.', 2006, 'Oldenburg',
        'Bibliotheks- u. Informationssystem d. Univ. Oldenburg', 148,
        'Gender; Geschlechtsspezifische Sozialisation; Adoleszenz; Junge; Geschlechterbeziehung; Geschlechterrolle; Weiblichkeit; Schule; Lehrer-Schüler-Beziehung; Schüler; Männlichkeit; Geschlechtsspezifik; Gleichbehandlung; Hierarchie; Konferenzschrift; Institution; Mädchen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Geschäftsbericht 2005.', 2006, 'Zürich', 'SWITCH', 34,
        'Forschung; Computerunterstützter Unterricht; Informatik; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Struwe, Ulrike', 'Akten-Özer, Dilek (Mitarb.); Salek-Schwarzte, Agnieszka (Mitarb.)',
        'IT-Ausbildung - und was dann?', 2006, 'Bielefeld', NULL, 100,
        'Chancengleichheit; Soziale Isolation; Frau; Informatik; Einkommen; Ausbildung; Arbeitsverhältnis; Berufliche Integration; Berufslaufbahn; Berufstätigkeit; Berufsverlauf; Frauenerwerbstätigkeit; Ausbildungsverwertung; Berufsvorbereitung; Berufswahl; Technischer Beruf; Ausbildungsverlauf; Handlungsspielraum; Technik; Mann; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Dücker, Sandra; Schapfel-Kaiser, Franz', NULL, 'IT-Technologie in der Berufsbildung.', 2006, 'Bonn',
        'Bundesinstitut für Berufsbildung', 12, 'Informationstechnik');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Lück-Schneider, Dagmar (Hrsg.); Maninger, Stephan (Hrsg.)', 'Wissensmanagement.', 2006, 'Brühl', NULL,
        231,
        'Wissen; Bildungsbeteiligung; Organisationspsychologie; Handlungstheorie; Lernpsychologie; Sozialpsychologie; Bildungspolitik; Bildungsökonomie; Interkulturelles Lernen; Lernen; Lernmotivation; Wissenstransfer; Lernkultur; Lernmethode; Lernziel; Lernorganisation; Informationstechnik; Humankapital; Humankapitaltheorie; Personalentwicklung; Personalinformationssystem; Personalmanagement; Privatwirtschaft; Unternehmenskultur; Arbeit; Arbeitsmarktentwicklung; Arbeitsplatz; Beschäftigungspolitik; Erwerbspersonenpotenzial; Höherqualifikation; Qualifizierung; Betriebliche Weiterbildung; Projektmanagement; Blended Learning; E-Learning; Prognose; Öffentliche Verwaltung; Informationsmanagement; Informationssystem; Informationsvermittlung; Managementinformationssystem; Wissensmanagement');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, '17 Millionen Studenten an den Hochschulen der Europäischen Union.', 2005, NULL, NULL, 11,
        'Erziehungswissenschaft; Bildungsbeteiligung; Lebensalter; Frau; Bildungsstatistik; Bildungsabschluss; Informatik; Mathematik; Ingenieurwissenschaft; Naturwissenschaften; Studium; Studienwahl; Studiendauer; Studienerfolg; Agrarwissenschaft; Geisteswissenschaften; Rechtswissenschaft; Sozialwissenschaften; Statistik; Studienfach; Studiengang; Akademiker; Hochschule; Hochschulsystem; Internationaler Vergleich; Europäische Union; Mann; Student; Studienanfänger');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Anforderungen und Verfahrensgrundsätze für die Akkreditierung von Bachelor- und Masterstudiengängen in den Ingenieurwissenschaften, der Architektur, der Informatik, den Naturwissenschaften und der Mathematik.',
        2005, NULL, NULL, 51,
        'Informatik; Mathematik; Ingenieurwissenschaft; Naturwissenschaften; Akkreditierung; Architektur; Studiengang; Master-Studiengang; Bachelor-Studiengang');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Geschäftsbericht 2004.', 2005, 'Zürich', 'SWITCH', 34,
        'Forschung; Computerunterstützter Unterricht; Informatik; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Moog, Horst', NULL, 'Informatik an Universitäten und Fachhochschulen.', 2005, 'Hannover', NULL, 114,
        'Informatik; Studium; Hochschulplanung; Hochschule; Monografie; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Stellungnahme zum Ausbauvorhaben der Fachhochschule für Technik und Wirtschaft (FHTW), Berlin.',
        2005, 'Köln', NULL, 77, 'Monografie; Berlin');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'BLK Informationen. 2004.', 2004, 'Bonn', 'BLK', 162,
        'Gleichstellung; Forschungspolitik; Bildungsplanung; BLK (Bund-Länder-Kommission für Bildungsplanung und Forschungsförderung); Förderungsmaßnahme; Modellversuch; Informatik; Rechtsgrundlage; Finanzierung; Fachhochschule; Studium; Wissenschaft; Juniorprofessur; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Rauner, Felix', NULL,
        'Die Berufsbildung im Berufsfeld Elektrotechnik-Informatik vor grundlegenden Weichenstellungen?', 2004,
        'Bremen', 'Institut Technik und Bildung', 16,
        'Lernfeld; Berufsausbildung; Ausbildungsinhalt; Elektroberuf; Elektrotechnischer Beruf; Informatiker; Arbeitsprozess; Neuordnung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Pastohr, Mandy; Wolter, Andrä', NULL, 'Die Zukunft des Humankapitals in Sachsen.', 2004, 'Dresden', NULL, 93,
        'Bildung; Soziale Herkunft; Ingenieurwissenschaft; Wechsel; Arbeitslosigkeit; Berufschance; Berufserfolg; Berufsbild; Studium; Studienberechtigter; Studienwahl; Studiendauer; Studienverlauf; Statistik; Studienfach; Akademiker; Abbruch; Monografie; Hochschulabsolvent; Studienanfänger; Deutschland; Sachsen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Puschmann, Andrea; Popp, Jacqueline; Krempkow, René', NULL, 'Dresdner Absolventenstudien 2003 Informatik.',
        2004, 'Dresden', NULL, 97,
        'Kompetenz; Zufriedenheit; Soziale Herkunft; Informatik; Beruf; Berufsanforderung; Berufseinmündung; Berufserfolg; Schlüsselqualifikation; Zusatzqualifikation; Bewerbung; Berufsfeld; Studium; Hochschulzugang; Hochschulzulassung; Aufbaustudium; Promotion; Studienverlauf; Zweitstudium; Übergang Studium - Beruf; Hochschule; Weiterbildung; Bewertung; Strategie; Hochschulabsolvent; Dresden; Sachsen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Stoltenburg, Frieder (Red.); Wüstenhagen, Doris (Red.)', 'Forschungsbericht 2002/2003.', 2004,
        'Wernigerode', NULL, 64, 'Monografie; Sachsen-Anhalt; Wernigerode');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Geschäftsbericht 2003.', 2004, 'Zürich', 'SWITCH', 34,
        'Forschung; Computerunterstützter Unterricht; Informatik; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Kursbuch eLearning 2004.', 2004, 'Bonn', 'DLP-Projektträger-Neue Medien i.d. Bildung+Fachinf.',
        400,
        'Neue Technologien; Bildungsangebot; Förderungsmaßnahme; Projekt; Studium; Hochschulunterricht; Hochschule; Fernstudium; Fernunterricht; E-Learning; Weiterbildung; Kursprogramm; Verzeichnis; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Gerber, S. (Hrsg.); Schötz, H.-P. (Hrsg.)',
        'Leistungspunkte- und Modulmanagement. Konzepte und Erfahrungen bei Bewertung, Anerkennung und Austausch von Modulen. Workshop am 23. und 24. März in Leipzig.',
        2004, 'Leipzig', 'Institut für Informatik, Universität Leipzig', 185,
        'Software; Berufsbild; Ingenieur; Übergang Studium - Beruf; Hochschule; Beschreibung; Leistungsindikator; Modul; Deutschland; Leipzig');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Pieper, Christine', NULL, 'Neue Disziplinen als Innovationsmotor?', 2004, 'München', NULL, 32,
        'Gesellschaft; Forschungspolitik; Computer; Computerprogramm; EDV; Informatik; Technologische Entwicklung; Innovationspolitik; Staat; Staatliche Einflussnahme; Staatliche Lenkung; Industrie; Wirtschaft; Wissenschaft; Wissenschaftspolitik; Hochschulwesen; Technische Hochschule; Innovation; Innovationsforschung; DDR; Deutschland; Deutschland-Westliche Länder');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schultz, Rita; Gerster, Hans-Dieter', NULL,
        'Schwierigkeiten beim Erwerb mathematischer Konzepte im Anfangsunterricht.', 2004, 'Freiburg, Breisgau',
        'Pädagogische Hochschule Freiburg, Institut für Mathematik und Informatik und ihre Didaktiken', 427,
        'Förderdiagnostik; Lernstandserhebung; Kind; Grundschule; Lernbehinderung; Mathematikunterricht; Rechenschwäche; Sonderpädagogik; Sonderschule');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Engelien, Martin (Hrsg.); Meißner, Klaus (Hrsg.)', 'Virtuelle Organisation und Neue Medien 2004.', 2004,
        'Lohmar', 'Eul', 431,
        'Bildung; Wissen; Wissensgesellschaft; Gemeinschaft; Identität; Vertrauen; Digitale Medien; Neue Medien; Virtuelle Gemeinschaft; Lernen; Lernumgebung; Wissenstransfer; Lerngemeinschaft; Didaktik; Lernplattform; Software; Datenverwaltung; Digitalisierung; Informatik; Computerunterstütztes Verfahren; Internationale Zusammenarbeit; Kleine und mittlere Unternehmen; Personalmanagement; Unternehmensorganisation; Wirtschaftspädagogik; Telearbeit; Freie Berufe; Virtuelles Unternehmen; Hochschullehre; Virtuelle Lehre; Hochschule; E-Learning; Web Based Training; Anwendung; Konferenzschrift; Kooperation; Netzwerk; Praxis; Virtualisierung; Internet; Betrieb; Team; Datenverarbeitung; Informationsmanagement; Informationssystem; Informationstechnologie; Online; Wissensmanagement; World Wide Web; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Pape, Bernd (Hrsg.); u.a.', 'Wissensprojekte.', 2004, 'Münster; München [u.a.]', 'Waxmann', 415,
        'Kooperatives Lernen; Informatik; Softwareergonomie; Studium; Hochschuldidaktik; E-Learning; Softwareentwicklung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Berghoff, Sonja', NULL, 'CHE Ländervergleich von Universitäten für 2003.', 2003, 'Gütersloh',
        'Centum für Hochschulentwicklung', 12,
        'Ländervergleich; Biologie; Chemie; Humanmedizin; Informatik; Ranking; Mathematik; Naturwissenschaften; Hochschule; Hochschulwesen; Baden-Württemberg; Bayern; Bremen; Deutschland; Thüringen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Einheitliche Prüfungsanforderungen Informatik.', 2003, 'Bonn', 'KMK', 71,
        'Kompetenz; Wissen; Abitur; Leistungsbeurteilung; Mündlicher Test; Prüfung; Prüfungsaufgabe; Schriftlicher Test; Beispiel; Grundkurs; Leistungskurs; Informatikunterricht; Qualifikation; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Paetow, Kai; Rovatsos, Michael', NULL, 'Grundlagen einer interaktionistischen Sozionik.', 2003, NULL, NULL, 36,
        'Techniksoziologie; Lernfähigkeit; Künstliche Intelligenz; Interaktion; Informatik; Soziologe');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Engbring, Dieter', NULL, 'Informatik im Herstellungs- und Nutzungskontext.', 2003, NULL, NULL, 213,
        'Gesellschaft; Angewandte Informatik; Informatik; Studium; Hochschuldidaktik; Dissertation; Online-Publikation');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schmiede, Rudi; Albert, Annette; Mürdter, Christoph; Schüttler, Dietmar; Stein, Ulrike', NULL,
        'IT an Hochschulen in Hessen: statistische Daten zu IT-Studiengängen und zu IT-Bestandteilen in den Curricula anderer Fächer.',
        2003, 'Darmstadt', NULL, 118,
        'Curriculum; Informatik; Informationstechnik; Fachkraft; Fachhochschule; Studium; Studiengang; Hochschule; Deutschland; Hessen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Küng, H.; u.a.', NULL,
        'Konzept der TaskForce ICT der Pädagogischen Hochschule Zentralschweiz zur Realisierung des Projekts ''Lehren und lernen mit Notebooks an der PHZ''.',
        2003, 'Luzern', 'PHZ', 14,
        'Lehrerbildung; Computerunterstützter Unterricht; Informatik; Pädagogische Hochschule; Konzept; Informationstechnologie; Zentralschweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schulte, Carsten', NULL, 'Lehr-Lernprozesse im Informatik-Anfangsunterricht.', 2003, NULL, NULL, 248,
        'Sekundarstufe II; Lehr-Lern-Prozess; Anfangsunterricht; Informatikunterricht; Objektorientierte Programmierung; Visualisieren; Dissertation; Modellierung; Objektorientierung; Online-Publikation');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schmiede, Rudi; Mürdter, Christoph; Nüchter, Oliver; Schmid, Alfons; Stein, Ulrike', NULL,
        'ProIT-Handlungsempfehlungen für allgemeinbildende Schulen, die duale Berufsausbildung, für Hochschulen und Weiterbildung.',
        2003, 'Darmstadt', NULL, 81,
        'Allgemein bildende Schule; Informatik; Branche; Berufsbildung; Arbeitskräftebedarf; Qualifikationsanforderung; Duales Ausbildungssystem; Fachkraft; Hochschule; Weiterbildung; Informationstechnologie; Deutschland; Hessen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Humbert, Ludger', NULL, 'Zur wissenschaftlichen Fundierung der Schulinformatik.', 2003, 'Witten', 'Pad-Verl.',
        284,
        'Sekundarstufe II; Lerntheorie; Informatik; Informatikunterricht; Wissenschaftstheorie; Dissertation; Modell');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Mayer, Beat; Messerli, Thomas; Grünig, Andreas', NULL,
        'Erhebung über die Informatik-Infrastruktur an Volksschulen.', 2002, 'Bern',
        'Erziehungsdirektion des Kantons Bern', 18,
        'Forschungsergebnis; Computer; Software; Hardware; Internet; Informationstechnologie; Bern; Kanton');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Forschungsevaluation an niedersächsischen Hochschulen und Forschungseinrichtungen.', 2002,
        'Hannover', NULL, 61,
        'Evaluation; Forschung; Forschungseinrichtung; Wissenstransfer; Standort; Informatik; Studium; Forschungskooperation; Forschungsleistung; Hochschulevaluation; Hochschule; Drittmittel; Interdisziplinarität; Kriterium; Monografie; Nachwuchsförderung; Qualität; Forschungspersonal; Braunschweig; Clausthal; Göttingen; Hannover; Niedersachsen; Oldenburg (Oldenburg); Osnabrück');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Huber, Maja; Cosandey, Florent; Bonfadelli, Heinz; Täube, Volker G.; Arvanitis, Spyros; Bütschi, Danielle; Hollenstein, Heinz; Marr, Mirko; Perret, Jean-François; Sabo, Müfit; Trechsel, Alexander H.',
        NULL, 'Informationsgesellschaft Schweiz.', 2002, 'Neuchâtel', 'BFS', 136,
        'Familie; Massenmedien; Bildungswesen; Bildungspolitik; Computerunterstützter Unterricht; Informatik; Geschäftskunde; Netzwerk; Internet; Betrieb; Verwaltung; Information; Informationsnetz; Informationstechnologie; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Plicht, Hannelore; Schreyer, Franziska', NULL,
        'Ingenieurinnen und Informatikerinnen: Schöne neue Arbeitswelt?', 2002, 'Nürnberg', 'IAB', 5,
        'Frau; Informatik; Ingenieurwissenschaft; Erwerbstätigkeit; Arbeitslosigkeit; Arbeitsmarktchance; Beruf; Berufsproblem; Informatiker; Ingenieur; Studienwahl; Studienfach; Akademiker; Geschlechterverteilung; Geschlechtsspezifik');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Sandfuchs, Gabriele; Stewart, Gerdi', NULL, 'Lehrberichte an bayerischen Universitäten.', 2002, 'München',
        NULL, 56,
        'Lehre; Internationalisierung; Lehrbericht; Hochschulforschung; Studentenbetreuung; Lehrevaluation; Hochschullehrer; Studentenberatung; Studienberatung; Studienangebot; Studienorganisation; Statistik; Studentische Bewertung; Forschungsbericht; Qualität; Student; Bayern');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Schiersmann, Christiane; Busse, Johannes; Krause, Detlev', NULL,
        'Medienkompetenz - Kompetenz für Neue Medien. Studie im Auftrag des Forum Bildung. Workshop am 14. September in Berlin.',
        2002, 'Bonn', 'Forum Bildung', 100,
        'Medienkompetenz; Medienpädagogik; Neue Medien; Bildungspolitik; Informatik; Konferenzschrift');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Modularisierung in Hochschulen.', 2002, 'Bonn', 'BLK', 88,
        'Bildungsangebot; Modellversuch; Informatik; Ingenieurwissenschaft; Studienreform; Geisteswissenschaften; Sozialwissenschaften; Wirtschaftswissenschaft; Hochschule; Empfehlung; Konferenzschrift; Modularisierung; Vernetzung; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Engelien, Martin (Hrsg.); u.a.', 'Virtuelle Organisation und Neue Medien 2002.', 2002, 'Lohmar u.a.',
        'Eul', 633,
        'Wissen; Wissensgesellschaft; Digitale Medien; Neue Medien; Virtuelle Gemeinschaft; Lernen; Lernumgebung; Lerngemeinschaft; Lernplattform; Datenverwaltung; Digitalisierung; Informatik; Computerunterstütztes Verfahren; Unternehmensorganisation; Wirtschaftspädagogik; Virtuelles Unternehmen; Hochschullehre; Virtuelle Lehre; Hochschule; E-Learning; Web Based Training; Anwendung; Anwendungsbeispiel; Konferenzschrift; Kooperation; Netzwerk; Organisation; Praxis; Tagungsbericht; Internet; Datenverarbeitung; Informationsmanagement; Informationssystem; Informationstechnologie; Online; Wissensmanagement; World Wide Web; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Grepper, Yvan; Döbeli, Beat', NULL,
        'Empfehlungen zu Beschaffung und Betrieb von Informatikmitteln an allgemeinbildenden Schulen.', 2001, 'Zürich',
        'ETHZ', 43,
        'Öffentliche Schule; Klassenraumeinteilung; Sekundarstufe I; Primarbereich; Computerunterstützter Unterricht; Hardware; Informatik; Finanzierung; Technische Hochschule; Internet; Zürich');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL,
        'Forschungsevaluation an niedersächsischen Hochschulen und Forschungseinrichtungen. Ingenieurwissenschaften. Berichte und Empfehlungen.',
        2001, 'Hannover', NULL, 92,
        'Evaluation; Forschung; Forschungseinrichtung; Wissenstransfer; Standort; Informationstechnik; Ingenieurwissenschaft; Maschinenbau; Architektur; Bautechnik; Elektrotechnik; Forschungskooperation; Forschungsleistung; Fachbereich; Hochschulevaluation; Hochschule; Drittmittel; Interdisziplinarität; Kriterium; Monografie; Nachwuchsförderung; Qualität; Forschungspersonal; Braunschweig; Clausthal; Hannover; Niedersachsen');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Kramer, Klaus-Dietrich (Red.); Draebecke, Leslie (Red.)', 'Forschungsbericht 2000/2001.', 2001,
        'Wernigerode', NULL, 56, 'Monografie; Halberstadt; Sachsen-Anhalt; Wernigerode');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Wolf, Claudia', 'Frauen - Technik - Evaluation.', 2001, 'Bonn', NULL, 48,
        'Chancengleichheit; Frau; Gleichstellung; Frauenförderung; Leistungsdifferenzierung; Indikator; Informatik; Ingenieurwissenschaft; Naturwissenschaften; Steuerung; Haushaltsmittel; Studium; Studienmotivation; Hochschule; Konferenzbericht; Kriterium; Nachwuchsförderung; Qualität; Sammelwerk; Technik; Verteilung; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, NULL, 'Geschäftsbericht 2000.', 2001, 'Zürich', 'SWITCH', 34,
        'Forschung; Computerunterstützter Unterricht; Informatik; Informationstechnik; Schweiz');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Minks, Karl-Heinz', NULL,
        'Ingenieurinnen und Naturwissenschaftlerinnen - neue Chancen zwischen Industrie- und Dienstleistungsgesellschaft.',
        2001, 'Hannover', 'Hochschul-Informations-System', 150,
        'Zukunftsperspektive; Zufriedenheit; Familie; Mutter; Vater; Frau; Elternzeit; Informatik; Mathematik; Maschinenbau; Naturwissenschaften; Branche; Einkommen; Nichterwerbstätigkeit; Zwischenbetriebliche Mobilität; Arbeitslosigkeit; Arbeitsplatzsicherheit; Arbeitsverhältnis; Beruf; Berufliche Stellung; Berufschance; Berufseinmündung; Berufsproblem; Berufssituation; Berufsverlauf; Karriere; Qualifikationsverwertung; Teilzeitbeschäftigung; Arbeitszeit; Architektur; Bautechnik; Elektrotechnik; Ingenieur; Männerberuf; Naturwissenschaftler; Fachhochschule; Universität; Aufbaustudium; Promotion; Akademiker; Weiterbildung; Umschulung; Familienarbeit; Berufliche Reintegration; Geschlechtsspezifik; Tätigkeitsfeld; Untersuchung; Techniker');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Diepold, Peter', NULL, 'Internet und Pädagogik.', 2001, 'Berlin', 'Humboldt-Universität', 35,
        'Begleituntersuchung; Bildungsserver; Lernen; Lehren; Lernsoftware; Informatik; Hochschule; E-Learning; Internet');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Tyroller, Alexandra', NULL, 'Jugendforum - Pfade nach Utopia.', 2001, 'Stuttgart',
        'Akademie für Technikfolgenabschätzung in Baden-Württemberg', 90,
        'Gesellschaft; Digitale Medien; Informations- und Kommunikationstechnologie; Sekundarstufe I; Sekundarstufe II; Informatik; Technikfolgenabschätzung');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Petersen, Ulrike (Hrsg.)',
        'Mentoring zwischen Universität und Forschung für Informatikerinnen (MUFFIN).', 2001, 'Sankt Augustin', NULL,
        54,
        'Forschungseinrichtung; Frau; Mentoring; Informatik; Personalentwicklung; Berufliche Integration; Berufsnachwuchs; Student');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES ('Klatt, Rüdiger; Gavriilidis, Konstantin; Kleinsimlinghaus, Kirsten; Feldmann, Maresa', NULL,
        'Nutzung elektronischer wissenschaftlicher Information in der Hochschulausbildung.', 2001, 'Dortmund', NULL,
        248,
        'Befragung; Multimedia; Neue Medien; Nutzerverhalten; Computer; Unterricht; Informationskompetenz; Hochschulbildung; Studium; Hochschullehrer; Hochschuldidaktik; Hochschule; Nutzung; Internet; Dekanat; Studierender; Fachinformation; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Engelien, Martin (Hrsg.); Homann, Jens (Hrsg.)',
        'Virtuelle Organisation und Neue Medien 2001. Workshop GeNeMe 2001, Gemeinschaften in Neuen Medien. TU Dresden, 27. und 28. September 2001.',
        2001, 'Lohmar; Köln', 'Josef Eul Verlag', 523,
        'Digitale Medien; Neue Medien; Virtuelle Gemeinschaft; Lernumgebung; Lernplattform; Datenverwaltung; Digitalisierung; Informatik; Softwaretechnologie; Computerunterstütztes Verfahren; Technologische Entwicklung; Unternehmen; Unternehmensorganisation; Wirtschaftspädagogik; Virtuelle Arbeitswelt; Virtuelles Unternehmen; Virtuelle Lehre; Hochschule; E-Learning; Web Based Training; Anwendung; Infrastruktur; Konferenzschrift; Netzwerk; Virtualisierung; Internet; Datenverarbeitung; Informationsmanagement; Informationssystem; Informationstechnologie; Online; Wissensmanagement; World Wide Web; Deutschland');
INSERT INTO books(authors, editors, title, year_of_publication, place_of_publication, publisher, pages, keywords)
VALUES (NULL, 'Engelien, Martin (Hrsg.); Neumann, Detlef (Hrsg.)',
        'Virtuelle Organisation und Neue Medien 2000. Workshop GeNeMe 2000, Gemeinschaften in Neuen Medien. TU Dresden, 5. und 6. Oktober 2000.',
        2000, 'Lohmar; Köln', 'Josef Eul Verlag', 387,
        'Digitale Medien; Medieneinsatz; Neue Medien; Virtuelle Gemeinschaft; Lernumgebung; Lernplattform; Digitalisierung; Informatik; Softwaretechnologie; Computerunterstütztes Verfahren; Technologische Entwicklung; Unternehmen; Unternehmensorganisation; Wirtschaftspädagogik; Virtuelle Arbeitswelt; Virtuelles Unternehmen; Hochschullehre; Studiengang; E-Learning; Web Based Training; Anwendung; Infrastruktur; Konferenzschrift; Netzwerk; Virtualisierung; Internet; Informationsmanagement; Informationssystem; Informationstechnologie; Online; World Wide Web; Deutschland');
