package de.tuberlin.sese.cabservice.persistence.cab.location;

import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownSectionException;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class CabLocationController {

    private final CabLocationService service;

    @GetMapping("/bookr/cabLocations")
    public List<CabLocationEntity> getCabLocations() {
        return service.getAllCabLocations();
    }

    @PostMapping("/ec/cabLocation")
    public ResponseEntity<?> setCabLocation(@RequestParam Long cabId, @RequestBody CabLocationEntity entity) {
        if (cabId == null || entity == null) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        entity.setCabId(cabId);
        try {
            service.saveCabLocation(entity);
        } catch (UnknownCabIdException | UnknownSectionException e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        return ResponseEntity.ok().build();
    }
}
