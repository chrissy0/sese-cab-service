package de.tuberlin.sese.cabservice.cab.registration;

import de.tuberlin.sese.cabservice.cab.registration.model.CabRegistrationIdModel;
import de.tuberlin.sese.cabservice.cab.registration.model.CabRegistrationModel;
import de.tuberlin.sese.cabservice.cab.registration.persistence.CabEntity;
import de.tuberlin.sese.cabservice.util.exceptions.NameAlreadyInUseException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownSectionException;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
}
