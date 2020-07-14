package de.tuberlin.sese.cabservice.util.exceptions;

public class UnknownCabLocationException extends IllegalArgumentException {

    public UnknownCabLocationException() {
    }

    public UnknownCabLocationException(String s) {
        super(s);
    }

    public UnknownCabLocationException(String message, Throwable cause) {
        super(message, cause);
    }

    public UnknownCabLocationException(Throwable cause) {
        super(cause);
    }
}
