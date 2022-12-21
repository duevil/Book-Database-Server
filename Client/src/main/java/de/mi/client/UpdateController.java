package de.mi.client;

import de.mi.common.BookFilter;

public class UpdateController extends Controller {
    private BookFilter filter = BookFilter.builder().build();

    @Override
    protected void initialize() {

    }

    @Override
    protected void onConnectionUpdate() {

    }

    public void setFilter(BookFilter filter) {
        this.filter = filter;
    }
}
