package de.tuberlin.sese.cabservice.persistence.customerinteraction.dropoff;

import de.tuberlin.sese.cabservice.util.exceptions.CabCustomerPositionConflictException;
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
public class DropoffController {

    private final DropoffService service;

    @PostMapping("/ec/requestDropoff")
    public ResponseEntity<?> requestDropoff(@RequestParam Long cabId, @RequestParam Long customerId) {
        try {
            service.dropoff(cabId, customerId);
        } catch (UnknownCabIdException | UnknownCabLocationException | UnknownJobIdException | CabCustomerPositionConflictException e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        return ResponseEntity.ok().build();
    }

    @GetMapping("/ec/dropoffsComplete")
    public ResponseEntity<?> dropoffsComplete(@RequestParam Long cabId) {
        DropoffCompleteModel dropoffCompleteModel;
        try {
            dropoffCompleteModel = service.dropoffsComplete(cabId);
        } catch (UnknownCabIdException e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        return ResponseEntity.ok().body(dropoffCompleteModel);
    }

    @GetMapping("/bookr/dropoffRequests")
    public ResponseEntity<?> dropoffRequests() {
        return ResponseEntity.ok().body(service.getDropoffRequests());
    }

    @PostMapping("/bookr/acceptDropoff")
    public ResponseEntity<?> acceptDropoff(@RequestParam Long customerId) {
        try {
            service.acceptDropoff(customerId);
        } catch (UnknownJobIdException e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        return ResponseEntity.ok().build();
    }
}
