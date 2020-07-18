package de.tuberlin.sese.cabservice.persistence.cab.blocked;

import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class CabBlockedController {

    private final CabBlockedService service;

    @PostMapping("/ec/blocked")
    public ResponseEntity<?> cabBlocked(@RequestParam Long cabId, @RequestParam Boolean blocked) {
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

    @GetMapping("/bookr/getBlocked")
    public ResponseEntity<?> getBlocked() {
        List<Long> blockedCabIds = service.getBlockedCabIds();
        return ResponseEntity.ok(blockedCabIds);
    }
}
