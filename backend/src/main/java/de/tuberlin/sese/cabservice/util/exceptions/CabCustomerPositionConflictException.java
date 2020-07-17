package de.tuberlin.sese.cabservice.util.exceptions;

public class CabCustomerPositionConflictException extends IllegalArgumentException {

    public CabCustomerPositionConflictException() {
    }

    public CabCustomerPositionConflictException(String s) {
        super(s);
    }

    public CabCustomerPositionConflictException(String message, Throwable cause) {
        super(message, cause);
    }

    public CabCustomerPositionConflictException(Throwable cause) {
        super(cause);
    }
}
