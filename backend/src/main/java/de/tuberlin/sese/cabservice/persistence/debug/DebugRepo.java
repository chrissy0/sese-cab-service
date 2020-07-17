package de.tuberlin.sese.cabservice.persistence.debug;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface DebugRepo extends CrudRepository<SensorStatusEntity, Long> {

}
