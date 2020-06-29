package de.tuberlin.sese.cabservice.persistence.route;

import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabLocationException;
import de.tuberlin.sese.cabservice.util.exceptions.VersionException;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class RouteController {

    private final RouteService service;

    @GetMapping("/ec/requestRoute")
    public ResponseEntity<?> getRoute(@RequestParam Long id, @RequestParam Integer version) {
        if (id == null || version == null) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        RouteEntity route;
        try {
            route = service.getRoute(id, version);
        } catch (VersionException | UnknownCabIdException | UnknownCabLocationException e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        } catch (Exception | Error e) {
            return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
        }

        return ResponseEntity.ok(route);
    }
}
