package de.tuberlin.sese.cabservice.cab.blocked;

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
public class CabBlockedController {

    private final CabBlockedService service;

    @PostMapping("/ec/blocked")
    public ResponseEntity<?> registerCab(@RequestParam Long cabId, @RequestParam Boolean blocked) {
        if (cabId == null || blocked == null) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        try {
            service.setBlocked(cabId, blocked);
        } catch (UnknownCabIdException e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        return ResponseEntity.ok().build();
    }
}
