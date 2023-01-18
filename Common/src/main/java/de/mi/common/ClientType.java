package de.mi.common;

public enum ClientType {
    BASIC, MASTER;

    public boolean isMaster() {
        return this == MASTER;
    }
}
