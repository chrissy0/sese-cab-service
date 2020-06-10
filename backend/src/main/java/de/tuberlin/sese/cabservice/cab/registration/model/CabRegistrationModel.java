package de.tuberlin.sese.cabservice.cab.registration.model;

import lombok.*;

@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class CabRegistrationModel {

    private String cabName;
    private Integer section;
}
