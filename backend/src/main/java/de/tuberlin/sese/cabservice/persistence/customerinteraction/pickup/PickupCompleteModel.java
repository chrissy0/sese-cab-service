package de.tuberlin.sese.cabservice.persistence.customerinteraction.pickup;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Getter
@Setter
public class PickupCompleteModel {

    private boolean complete;
}
