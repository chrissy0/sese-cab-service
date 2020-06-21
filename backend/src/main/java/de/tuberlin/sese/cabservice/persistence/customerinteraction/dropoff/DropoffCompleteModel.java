package de.tuberlin.sese.cabservice.persistence.customerinteraction.dropoff;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Getter
@Setter
public class DropoffCompleteModel {

    private boolean complete;
}
