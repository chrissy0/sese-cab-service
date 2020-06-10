package de.tuberlin.sese.cabservice.cabmanagement.cablocation;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class CabLocationService {

    private final CabLocationRepo repo;

    public List<CabLocationEntity> getAllCabLocations() {
        List<CabLocationEntity> entities = new LinkedList<>();
        repo.findAll().forEach(entities::add);
        return entities;
    }

    public void saveCabLocation(CabLocationEntity newEntity) {
        repo.findById(newEntity.getCabId());
        repo.save(newEntity);
    }
}
