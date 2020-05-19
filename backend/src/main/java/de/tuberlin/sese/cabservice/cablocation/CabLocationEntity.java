package de.tuberlin.sese.cabservice.cablocation;

import lombok.*;

import javax.persistence.Entity;
import javax.persistence.Id;

@Entity
@Builder
@Getter
@NoArgsConstructor
@AllArgsConstructor
public class CabLocationEntity {

    @Id
    private Long cabId;
    private String cabName;
    private int section;
}
