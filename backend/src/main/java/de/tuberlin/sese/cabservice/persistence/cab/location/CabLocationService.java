package de.tuberlin.sese.cabservice.persistence.cab.location;

import com.google.common.base.Preconditions;
import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownSectionException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class CabLocationService {

    private final CabLocationRepo locationRepo;

    private final CabRepo cabRepo;

    public Optional<CabLocationEntity> getCabLocation(Long id) {
        return locationRepo.findById(id);
    }

    public List<CabLocationEntity> getAllCabLocations() {
        List<CabLocationEntity> entities = new LinkedList<>();
        locationRepo.findAll().forEach(entities::add);
        return entities;
    }

    public void saveCabLocation(CabLocationEntity newEntity) {
        validateEntity(newEntity);

        locationRepo.findById(newEntity.getCabId());
        locationRepo.save(newEntity);
    }

    private void validateEntity(CabLocationEntity entity) {
        Preconditions.checkArgument(entity != null, "CabLocationEntity was null");
        Preconditions.checkArgument(entity.getCabId() != null, "CabLocationEntity ID was null");
        Preconditions.checkArgument(entity.getSection() != null, "CabLocationEntity Section was null");

        if (!cabRepo.findById(entity.getCabId()).isPresent()) {
            throw new UnknownCabIdException("No cab with ID " + entity.getCabId() + "is known");
        }

        Integer section = entity.getSection();
        if (section < 0 || section > 15) {
            throw new UnknownSectionException("Section \"" + section + "\" is unknown");
        }
    }
}
