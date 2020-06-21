package de.tuberlin.sese.cabservice.persistence.customerinteraction.pickup;

import de.tuberlin.sese.cabservice.util.exceptions.CabCustomerPositionConflict;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabLocationException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownJobIdException;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class PickupController {

    private final PickupService service;

    @PostMapping("/ec/requestPickup")
    public ResponseEntity<?> requestPickup(@RequestParam Long cabId, @RequestParam Long customerId) {
        try {
            service.pickup(cabId, customerId);
        } catch (UnknownCabIdException | UnknownCabLocationException | UnknownJobIdException | CabCustomerPositionConflict e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        return ResponseEntity.ok().build();
    }

    @GetMapping("/ec/pickupsComplete")
    public ResponseEntity<?> pickupsComplete(@RequestParam Long cabId) {
        PickupCompleteModel pickupCompleteModel;
        try {
            pickupCompleteModel = service.pickupsComplete(cabId);
        } catch (UnknownCabIdException e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        return ResponseEntity.ok().body(pickupCompleteModel);
    }

    @GetMapping("/bookr/pickupRequests")
    public ResponseEntity<?> pickupRequests() {
        return ResponseEntity.ok().body(service.getPickupRequests());
    }

    @PostMapping("/bookr/acceptPickup")
    public ResponseEntity<?> acceptPickup(@RequestParam Long customerId) {
        try {
            service.acceptPickup(customerId);
        } catch (UnknownJobIdException e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        return ResponseEntity.ok().build();
    }
}
