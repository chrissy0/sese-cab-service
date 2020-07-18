package de.tuberlin.sese.cabservice.persistence.cab.dysfunctional;

import lombok.*;

import javax.persistence.Entity;
import javax.persistence.Id;

@Entity
@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class CabDysfunctionalEntity {

    @Id
    private Long cabId;
}
