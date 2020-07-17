package de.tuberlin.sese.cabservice.util.exceptions;

public class VersionException extends RuntimeException {

    public VersionException() {
    }

    public VersionException(String s) {
        super(s);
    }

    public VersionException(String message, Throwable cause) {
        super(message, cause);
    }

    public VersionException(Throwable cause) {
        super(cause);
    }
}
