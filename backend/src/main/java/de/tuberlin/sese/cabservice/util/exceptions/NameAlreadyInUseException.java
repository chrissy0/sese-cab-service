package de.tuberlin.sese.cabservice.util.exceptions;

public class NameAlreadyInUseException extends IllegalArgumentException {

    public NameAlreadyInUseException() {
    }

    public NameAlreadyInUseException(String s) {
        super(s);
    }

    public NameAlreadyInUseException(String message, Throwable cause) {
        super(message, cause);
    }

    public NameAlreadyInUseException(Throwable cause) {
        super(cause);
    }
}
