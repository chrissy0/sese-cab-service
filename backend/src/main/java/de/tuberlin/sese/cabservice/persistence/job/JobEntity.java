package de.tuberlin.sese.cabservice.persistence.job;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import java.time.LocalDateTime;

@Entity
@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class JobEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Long customerId;
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private CustomerState customerState;
    private int start;
    private int end;
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private LocalDateTime timestamp;
    private boolean inProgress;

    public enum CustomerState {
        WAITING,
        IN_CAB,
        AT_DESTINATION
    }
}
