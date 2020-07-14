package de.tuberlin.sese.cabservice.persistence.job;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface JobRepo extends CrudRepository<JobEntity, Long> {

}
