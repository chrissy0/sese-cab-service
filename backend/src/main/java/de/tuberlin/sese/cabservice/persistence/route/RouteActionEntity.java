package de.tuberlin.sese.cabservice.persistence.route;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

import static com.fasterxml.jackson.annotation.JsonInclude.Include.NON_NULL;

@Entity
@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(NON_NULL)
public class RouteActionEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private Action action;
    private Direction direction;
    private Integer marker;
    private Long customerId;

    enum Action {
        @JsonProperty("turn")
        TURN,
        @JsonProperty("pickup")
        PICKUP,
        @JsonProperty("dropoff")
        DROPOFF,
        @JsonProperty("wait")
        WAIT
    }

    enum Direction {
        @JsonProperty("left")
        LEFT,
        @JsonProperty("right")
        RIGHT
    }

    @Override
    public String toString() {
        if (action.equals(Action.TURN)) {
            return marker + " turn " + direction;
        }
        if (action.equals(Action.PICKUP) || action.equals(Action.DROPOFF)) {
            return marker + " " + action + " customer " + customerId;
        }
        if (action.equals(Action.WAIT)) {
            return marker + " " + action;
        }
        return null;
    }
}
