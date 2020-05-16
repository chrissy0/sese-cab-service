package de.tuberlin.sese.cabservice.cablocation;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class CabLocationController {

    private final CabLocationService service;

    @GetMapping("/cab-locations")
    public List<CabLocationEntity> getCabLocation() {
        return service.getAllCabLocations();
    }

    @PostMapping("/cab-location")
    public void setCabLocation(@RequestBody CabLocationEntity entity) {
        service.saveCabLocation(entity);
    }
}
