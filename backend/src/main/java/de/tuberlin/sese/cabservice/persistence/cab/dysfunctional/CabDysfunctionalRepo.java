package de.tuberlin.sese.cabservice.persistence.cab.dysfunctional;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CabDysfunctionalRepo extends CrudRepository<CabDysfunctionalEntity, Long> {

}
