package de.tuberlin.sese.cabservice.cabmanagement.cablocation;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CabLocationRepo extends CrudRepository<CabLocationEntity, Long> {

}
