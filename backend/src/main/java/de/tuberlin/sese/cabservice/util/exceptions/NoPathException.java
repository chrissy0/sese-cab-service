package de.tuberlin.sese.cabservice.util.exceptions;

public class NoPathException extends Exception {

    public NoPathException() {
    }

    public NoPathException(String s) {
        super(s);
    }

    public NoPathException(String message, Throwable cause) {
        super(message, cause);
    }

    public NoPathException(Throwable cause) {
        super(cause);
    }
}
