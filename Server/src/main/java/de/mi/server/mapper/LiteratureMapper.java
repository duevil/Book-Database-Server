package de.mi.server.mapper;

import de.mi.common.Author;
import de.mi.common.Book;
import de.mi.common.Subfield;

public final class LiteratureMapper {
    public static final Mapper<Book> BOOK_MAPPER = new BookMapper();
    public static final Mapper<Subfield> SUBFIELD_MAPPER = new SubfieldMapper();
    public static final Mapper<Author> AUTHOR_MAPPER = new AuthorMapper();

    private LiteratureMapper() {
    }
}
