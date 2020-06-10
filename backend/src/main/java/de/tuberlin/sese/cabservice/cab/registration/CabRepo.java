package de.tuberlin.sese.cabservice.cab.registration;

import de.tuberlin.sese.cabservice.cab.registration.persistence.CabEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CabRepo extends CrudRepository<CabEntity, Long> {

}
