package de.mi.common;

/**
 * Auflistung der existierenden Client-Arten
 *
 * @author Malte Kasolowsky <code>m30114</code>
 */
public enum ClientType {
    BASIC, MASTER;

    /**
     * Prüft, ob die jeweilige Client-Art ein Haupt-Nutzer mit entsprechenden Rechten ist
     *
     * @return true, wenn die Client-Art ein MASTER-Client ist
     */
    public boolean isMaster() {
        return this == MASTER;
    }
}
