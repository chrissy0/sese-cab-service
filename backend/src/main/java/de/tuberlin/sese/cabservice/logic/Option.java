package de.tuberlin.sese.cabservice.logic;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Getter
@Setter
public class Option {

    private Integer toSection;
    private Integer fromSection;
    private Direction direction;

    public enum Direction {
        LEFT,
        RIGHT,
        STRAIGHT
    }
}
