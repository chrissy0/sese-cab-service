package de.tuberlin.sese.cabservice.persistence.cab.blocked;

import com.google.common.base.Preconditions;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationEntity;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationService;
import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import static com.google.common.collect.Streams.stream;

@Service
@RequiredArgsConstructor
public class CabBlockedService {

    private final CabRepo cabRepo;

    private final CabBlockedRepo blockedRepo;

    private final CabLocationService locationService;

    public List<Integer> getBlockedSections() {

        return stream(blockedRepo.findAll())
                .map(CabBlockedEntity::getCabId)
                .map(cabId -> locationService.getCabLocation(cabId).orElse(null))
                .filter(Objects::nonNull)
                .map(CabLocationEntity::getSection)
                .collect(Collectors.toList());
    }

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

    public boolean isBlocked(Long cabId) {
        validateCabId(cabId);
        return blockedRepo.findById(cabId).isPresent();
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

    public List<Long> getBlockedCabIds() {
        return stream(blockedRepo.findAll())
                .map(CabBlockedEntity::getCabId)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }
}
