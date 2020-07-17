package de.tuberlin.sese.cabservice.persistence.debug;

import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownSensorNameException;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class DebugController {

    private final DebugService service;

    @GetMapping("/ec/sensorStatus")
    public ResponseEntity<?> getSensorStatus(@RequestParam Long cabId) {
        CabSensorStatus cabSensorStatus;
        try {
            cabSensorStatus = service.getCabSensorStatus(cabId);
        } catch (UnknownCabIdException e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        return ResponseEntity.ok(cabSensorStatus);
    }

    @PostMapping("/bookr/setSensorStatus")
    public ResponseEntity<?> setSensorStatus(@RequestParam Long cabId, @RequestParam String sensorName, @RequestParam boolean disabled, @RequestParam int noise) {
        try {
            service.saveSensorStatus(cabId, sensorName, disabled, noise);
        } catch (UnknownCabIdException | UnknownSensorNameException e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        return ResponseEntity.ok().build();
    }
}
