package de.tuberlin.sese.cabservice.persistence.debug;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

import static com.fasterxml.jackson.annotation.JsonInclude.Include.NON_NULL;

@Builder
@Getter
@Setter
@JsonInclude(NON_NULL)
public class CabSensorStatus {

    private boolean sensorErrors;
    private List<SensorStatusEntity> sensors;

    public boolean hasSensorErrors() {
        return sensorErrors;
    }
}
