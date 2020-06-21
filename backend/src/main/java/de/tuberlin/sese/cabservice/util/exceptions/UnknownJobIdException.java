package de.tuberlin.sese.cabservice.util.exceptions;

public class UnknownJobIdException extends IllegalArgumentException {

    public UnknownJobIdException() {
    }

    public UnknownJobIdException(String s) {
        super(s);
    }

    public UnknownJobIdException(String message, Throwable cause) {
        super(message, cause);
    }

    public UnknownJobIdException(Throwable cause) {
        super(cause);
    }
}
