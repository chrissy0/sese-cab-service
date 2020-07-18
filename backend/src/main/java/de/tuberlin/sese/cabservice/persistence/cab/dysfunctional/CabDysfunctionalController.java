package de.tuberlin.sese.cabservice.persistence.cab.dysfunctional;

import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class CabDysfunctionalController {

    private final CabDysfunctionalService service;

    @PostMapping("/ec/functional")
    public ResponseEntity<?> cabFunctional(@RequestParam Long cabId, @RequestParam Boolean functional) {
        if (cabId == null || functional == null) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        boolean dysfunctional = !functional;

        try {
            service.setDysfunctional(cabId, dysfunctional);
        } catch (UnknownCabIdException e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        return ResponseEntity.ok().build();
    }
}
