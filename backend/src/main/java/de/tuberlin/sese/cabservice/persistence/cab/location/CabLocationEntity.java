package de.tuberlin.sese.cabservice.persistence.cab.location;

import lombok.*;

import javax.persistence.Entity;
import javax.persistence.Id;

@Entity
@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class CabLocationEntity {

    @Id
    private Long cabId;
    private Integer section;
}
