package de.tuberlin.sese.cabservice.util.exceptions;

public class CabCustomerPositionConflict extends IllegalArgumentException {

    public CabCustomerPositionConflict() {
    }

    public CabCustomerPositionConflict(String s) {
        super(s);
    }

    public CabCustomerPositionConflict(String message, Throwable cause) {
        super(message, cause);
    }

    public CabCustomerPositionConflict(Throwable cause) {
        super(cause);
    }
}
