package de.tuberlin.sese.cabservice.logic;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Getter
@Setter
public class Option {

    private Integer section;
    private Direction direction;

    public enum Direction {
        LEFT,
        RIGHT,
        STRAIGHT
    }
}
