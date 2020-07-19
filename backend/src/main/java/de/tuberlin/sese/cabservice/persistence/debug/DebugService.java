package de.tuberlin.sese.cabservice.persistence.debug;

import com.google.common.base.Preconditions;
import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownSensorNameException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static com.google.common.collect.Streams.stream;

@Service
@RequiredArgsConstructor
@Slf4j
public class DebugService {

    private final DebugRepo debugRepo;

    private final CabRepo cabRepo;

    public void saveSensorStatus(Long cabId, String sensorName, boolean disabled, int noise) {
        validateCabId(cabId);
        validateSensorName(sensorName);

        saveSensorStatus(SensorStatusEntity.builder()
                .cabId(cabId)
                .name(sensorName)
                .disabled(disabled)
                .noise(noise)
                .build());
    }

    private void saveSensorStatus(SensorStatusEntity entity) {
        Optional<SensorStatusEntity> repoSensorStatusOptional = stream(debugRepo.findAll())
                .filter(repoEntity -> repoEntity.getCabId().equals(entity.getCabId()))
                .filter(repoEntity -> repoEntity.getName().equals(entity.getName()))
                .findFirst();

        if (!entity.isDisabled() && entity.getNoise() == 0) {
            repoSensorStatusOptional.ifPresent(sensorStatusEntity -> {
                log.info(String.format("Deleting sensor status for Cab ID %d and sensor name '%s'", entity.getCabId(), entity.getName()));
                debugRepo.delete(sensorStatusEntity);
            });
            return;
        }

        log.info(String.format("Setting sensor status for Cab ID %d and sensor name '%s' to disabled: %s and noise %d", entity.getCabId(), entity.getName(), entity.isDisabled(), entity.getNoise()));

        if (repoSensorStatusOptional.isPresent()) {
            SensorStatusEntity sensorStatusEntity = repoSensorStatusOptional.get();

            sensorStatusEntity.setDisabled(entity.isDisabled());
            sensorStatusEntity.setNoise(entity.getNoise());
            debugRepo.save(sensorStatusEntity);
        } else {
            debugRepo.save(entity);
        }
    }

    public CabSensorStatus getCabSensorStatus(Long cabId) {
        validateCabId(cabId);

        List<SensorStatusEntity> sensorStatusEntities = getSensorStatusEntities(cabId);

        return CabSensorStatus.builder()
                .sensorErrors(!sensorStatusEntities.isEmpty())
                .sensors(sensorStatusEntities.isEmpty() ? null : sensorStatusEntities)
                .build();
    }

    private List<SensorStatusEntity> getSensorStatusEntities(Long cabId) {
        validateCabId(cabId);

        return stream(debugRepo.findAll())
                .filter(repoEntity -> repoEntity.getCabId().equals(cabId))
                .collect(Collectors.toList());
    }

    private void validateCabId(Long cabId) {
        Preconditions.checkArgument(cabId != null, "Cab ID was null");

        if (!cabRepo.findById(cabId).isPresent()) {
            throw new UnknownCabIdException("Cab ID \"" + cabId + "\" is unknown");
        }
    }

    private void validateSensorName(String sensorName) {
        if (sensorName.equals("inf_right")
                || sensorName.equals("inf_right2")
                || sensorName.equals("inf_cent")
                || sensorName.equals("inf_cent2")
                || sensorName.equals("inf_rm_fr")
                || sensorName.equals("inf_rm_fr2")
                || sensorName.equals("inf_rm_fr_act")
                || sensorName.equals("inf_rm_fr_act2")
                || sensorName.equals("inf_rm_fl")
                || sensorName.equals("inf_rm_fl2")
                || sensorName.equals("inf_rm_fl_act")
                || sensorName.equals("inf_rm_fl_act2")
                || sensorName.equals("inf_rm_bl")
                || sensorName.equals("inf_rm_bl2")
                || sensorName.equals("inf_rm_bl_act")
                || sensorName.equals("inf_rm_bl_act2")
                || sensorName.equals("inf_rm_br")
                || sensorName.equals("inf_rm_br2")
                || sensorName.equals("inf_rm_br_act")
                || sensorName.equals("inf_rm_br_act2")
                || sensorName.equals("dist_c")
                || sensorName.equals("dist_c2")
                || sensorName.equals("curb_rf")
                || sensorName.equals("curb_rf2")
                || sensorName.equals("curb_lf")
                || sensorName.equals("curb_lf2")
                || sensorName.equals("dist_r")
                || sensorName.equals("dist_r2")
                || sensorName.equals("dist_l")
                || sensorName.equals("dist_l2")
                || sensorName.equals("inf_left")
                || sensorName.equals("inf_left2")
                || sensorName.equals("wheel1")
                || sensorName.equals("wheel2")
                || sensorName.equals("wheel3")
                || sensorName.equals("wheel4")) {
            return;
        }
        throw new UnknownSensorNameException("Sensor " + sensorName + " does not exist.");
    }
}
