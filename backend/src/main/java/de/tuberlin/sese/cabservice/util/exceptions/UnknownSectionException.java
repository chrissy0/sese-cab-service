package de.tuberlin.sese.cabservice.util.exceptions;

public class UnknownSectionException extends IllegalArgumentException {

    public UnknownSectionException() {
    }

    public UnknownSectionException(String s) {
        super(s);
    }

    public UnknownSectionException(String message, Throwable cause) {
        super(message, cause);
    }

    public UnknownSectionException(Throwable cause) {
        super(cause);
    }
}
