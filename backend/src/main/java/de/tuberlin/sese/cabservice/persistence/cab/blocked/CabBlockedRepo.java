package de.tuberlin.sese.cabservice.persistence.cab.blocked;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CabBlockedRepo extends CrudRepository<CabBlockedEntity, Long> {

}
