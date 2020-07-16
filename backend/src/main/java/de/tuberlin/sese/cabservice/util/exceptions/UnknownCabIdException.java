package de.tuberlin.sese.cabservice.util.exceptions;

public class UnknownCabIdException extends IllegalArgumentException {

    public UnknownCabIdException() {
    }

    public UnknownCabIdException(String s) {
        super(s);
    }

    public UnknownCabIdException(String message, Throwable cause) {
        super(message, cause);
    }

    public UnknownCabIdException(Throwable cause) {
        super(cause);
    }
}
