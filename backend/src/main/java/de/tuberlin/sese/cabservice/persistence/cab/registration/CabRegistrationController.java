package de.tuberlin.sese.cabservice.persistence.cab.registration;

import de.tuberlin.sese.cabservice.persistence.cab.registration.model.CabRegistrationIdModel;
import de.tuberlin.sese.cabservice.persistence.cab.registration.model.CabRegistrationModel;
import de.tuberlin.sese.cabservice.persistence.cab.registration.persistence.CabEntity;
import de.tuberlin.sese.cabservice.util.exceptions.NameAlreadyInUseException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownSectionException;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class CabRegistrationController {

    private final CabRegistrationService cabRegistrationService;

    @PostMapping("/ec/registerCab")
    public ResponseEntity<?> registerCab(@RequestBody CabRegistrationModel model) {
        if (model == null) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        CabEntity cabEntity = CabEntity.builder()
                .name(model.getCabName())
                .build();

        long cabId;
        try {
            cabId = cabRegistrationService.registerCab(cabEntity, model.getSection());
        } catch (NameAlreadyInUseException | UnknownSectionException e) {
            return new ResponseEntity<>(HttpStatus.CONFLICT);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
        }

        return ResponseEntity.ok(CabRegistrationIdModel.builder().id(cabId).build());
    }

    @GetMapping("/bookr/registeredCabs")
    public ResponseEntity<?> registeredCabs() {
        return ResponseEntity.ok(cabRegistrationService.registeredCabs());
    }
}
