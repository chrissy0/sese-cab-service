package de.tuberlin.sese.cabservice.persistence.customerinteraction.pickup;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Getter
@Setter
public class PickupCompleteModel {

    @JsonProperty("completed")
    private boolean complete;
}
