package de.tuberlin.sese.cabservice.util.exceptions;

public class UnknownSensorNameException extends IllegalArgumentException {

    public UnknownSensorNameException(String s) {
        super(s);
    }
}
