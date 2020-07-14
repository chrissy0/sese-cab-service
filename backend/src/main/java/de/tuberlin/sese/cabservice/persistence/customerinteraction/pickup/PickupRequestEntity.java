package de.tuberlin.sese.cabservice.persistence.customerinteraction.pickup;

import lombok.*;

import javax.persistence.Entity;
import javax.persistence.Id;

@Entity
@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class PickupRequestEntity {

    @Id
    private Long customerId;
    private Long cabId;
}
