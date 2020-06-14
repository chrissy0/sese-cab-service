package de.tuberlin.sese.cabservice.cab.blocked;

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
