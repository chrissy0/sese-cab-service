package de.tuberlin.sese.cabservice.cab.blocked;

import com.google.common.base.Preconditions;
import de.tuberlin.sese.cabservice.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@RequiredArgsConstructor
public class CabBlockedService {

    private final CabRepo cabRepo;

    private final CabBlockedRepo blockedRepo;

    public void setBlocked(Long cabId, Boolean blocked) {
        validateCabId(cabId);
        validateBlocked(blocked);

        Optional<CabBlockedEntity> entityOptional = blockedRepo.findById(cabId);
        if (blocked) {
            blockedRepo.save(CabBlockedEntity.builder()
                    .cabId(cabId)
                    .build());
        } else {
            entityOptional.ifPresent(blockedRepo::delete);
        }

    }

    private void validateBlocked(Boolean blocked) {
        Preconditions.checkArgument(blocked != null, "\"blocked\" was null");
    }

    private void validateCabId(Long cabId) {
        Preconditions.checkArgument(cabId != null, "Cab ID was null");

        if (!cabRepo.findById(cabId).isPresent()) {
            throw new UnknownCabIdException("Cab ID \"" + cabId + "\" is unknown");
        }
    }
}
