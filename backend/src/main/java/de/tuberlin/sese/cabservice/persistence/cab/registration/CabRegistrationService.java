package de.tuberlin.sese.cabservice.persistence.cab.registration;

import com.google.common.base.Preconditions;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationEntity;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationService;
import de.tuberlin.sese.cabservice.persistence.cab.registration.persistence.CabEntity;
import de.tuberlin.sese.cabservice.util.exceptions.NameAlreadyInUseException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownSectionException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

import static com.google.common.collect.Streams.stream;

@Service
@RequiredArgsConstructor
public class CabRegistrationService {

    private final CabRepo repo;

    private final CabLocationService locationService;

    public long registerCab(CabEntity entity, Integer section) {
        validateEntity(entity);
        validateSection(section);

        Long cabId = repo.save(entity).getId();

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(section)
                .build());

        return cabId;
    }

    private void validateSection(Integer section) {
        Preconditions.checkArgument(section != null, "Section was null");

        if (section < 0 || section > 15) {
            throw new UnknownSectionException("Section \"" + section + "\" is unknown");
        }
    }

    private void validateEntity(CabEntity entity) {
        Preconditions.checkArgument(entity != null, "CabEntity was null");
        Preconditions.checkArgument(entity.getId() == null, "Id of CabEntity was not null");
        Preconditions.checkArgument(entity.getName() != null && !entity.getName().isEmpty(), "Name of CabEntity was null or empty");

        repo.findAll().forEach(cabEntity -> {
            if (entity.getName().equals(cabEntity.getName())) {
                throw new NameAlreadyInUseException("CabName \"" + entity.getName() + "\" is already in use");
            }
        });
    }

    public List<CabEntity> registeredCabs() {
        return stream(repo.findAll()).collect(Collectors.toList());
    }
}
