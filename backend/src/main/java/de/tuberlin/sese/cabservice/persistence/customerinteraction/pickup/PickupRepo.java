package de.tuberlin.sese.cabservice.persistence.customerinteraction.pickup;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface PickupRepo extends CrudRepository<PickupRequestEntity, Long> {

}
