package de.tuberlin.sese.cabservice.logic;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

/**
 * Multiple options make up a PathFinder route
 */
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
        STRAIGHT,
        WAIT
    }
}
