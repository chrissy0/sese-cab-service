package de.tuberlin.sese.cabservice.route;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface RouteRepo extends CrudRepository<RouteEntity, Long> {

}
