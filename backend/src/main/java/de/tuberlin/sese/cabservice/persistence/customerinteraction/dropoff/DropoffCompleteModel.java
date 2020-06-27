package de.tuberlin.sese.cabservice.persistence.customerinteraction.dropoff;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Getter
@Setter
public class DropoffCompleteModel {

    @JsonProperty("completed")
    private boolean complete;
}
