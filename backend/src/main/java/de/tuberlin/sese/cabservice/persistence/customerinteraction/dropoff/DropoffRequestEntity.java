package de.tuberlin.sese.cabservice.persistence.customerinteraction.dropoff;

import lombok.*;

import javax.persistence.Entity;
import javax.persistence.Id;

@Entity
@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DropoffRequestEntity {

    @Id
    private Long customerId;
    private Long cabId;
}
