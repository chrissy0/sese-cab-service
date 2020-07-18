package de.tuberlin.sese.cabservice.persistence.cab.dysfunctional;

import com.google.common.base.Preconditions;
import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@RequiredArgsConstructor
public class CabDysfunctionalService {

    private final CabRepo cabRepo;

    private final CabDysfunctionalRepo dysfunctionalRepo;

    public void setDysfunctional(Long cabId, Boolean dysfunctional) {
        validateCabId(cabId);
        validateDysfunctional(dysfunctional);

        Optional<CabDysfunctionalEntity> entityOptional = dysfunctionalRepo.findById(cabId);
        if (dysfunctional) {
            dysfunctionalRepo.save(CabDysfunctionalEntity.builder()
                    .cabId(cabId)
                    .build());
        } else {
            entityOptional.ifPresent(dysfunctionalRepo::delete);
        }
    }

    public boolean isDysfunctional(Long cabId) {
        validateCabId(cabId);
        return dysfunctionalRepo.findById(cabId).isPresent();
    }

    private void validateDysfunctional(Boolean dysfunctional) {
        Preconditions.checkArgument(dysfunctional != null, "\"dysfunctional\" was null");
    }

    private void validateCabId(Long cabId) {
        Preconditions.checkArgument(cabId != null, "Cab ID was null");

        if (!cabRepo.findById(cabId).isPresent()) {
            throw new UnknownCabIdException("Cab ID \"" + cabId + "\" is unknown");
        }
    }
}
