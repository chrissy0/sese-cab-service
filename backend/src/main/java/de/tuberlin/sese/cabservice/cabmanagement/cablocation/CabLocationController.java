package de.tuberlin.sese.cabservice.cabmanagement.cablocation;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class CabLocationController {

    private final CabLocationService service;

    @GetMapping("/cab-locations")
    public List<CabLocationEntity> getCabLocations() {
        return service.getAllCabLocations();
    }

    @PostMapping("/ec/cab-location")
    public void setCabLocation(@RequestParam Long cabId, @RequestBody CabLocationEntity entity) {
        entity.setCabId(cabId);
        service.saveCabLocation(entity);
    }
}
