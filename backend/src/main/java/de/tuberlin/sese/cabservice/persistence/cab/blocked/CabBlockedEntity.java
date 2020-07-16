package de.tuberlin.sese.cabservice.persistence.cab.blocked;

import lombok.*;

import javax.persistence.Entity;
import javax.persistence.Id;

@Entity
@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class CabBlockedEntity {

    @Id
    private Long cabId;
}
