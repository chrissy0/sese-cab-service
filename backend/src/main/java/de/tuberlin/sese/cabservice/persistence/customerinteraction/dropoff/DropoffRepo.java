package de.tuberlin.sese.cabservice.persistence.customerinteraction.dropoff;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface DropoffRepo extends CrudRepository<DropoffRequestEntity, Long> {

}
