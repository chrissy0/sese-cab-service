package de.tuberlin.sese.cabservice.route;

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
    private Integer customerId;

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
}
