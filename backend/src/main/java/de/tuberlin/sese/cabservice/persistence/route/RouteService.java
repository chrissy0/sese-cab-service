package de.tuberlin.sese.cabservice.persistence.route;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;

import static de.tuberlin.sese.cabservice.persistence.route.RouteActionEntity.Action.WAIT;
import static java.util.Collections.singletonList;

@Service
@RequiredArgsConstructor
public class RouteService {

    private final RouteRepo repo;

    public RouteEntity getRoute(Long cabId, Integer version) {
        // TODO Placeholder code
        // TODO Only allow registered cabs to request a route

        Optional<RouteEntity> routeOptional = repo.findById(cabId);

        if (routeOptional.isPresent()) {
            RouteEntity route = routeOptional.get();
            route.setVersion(route.getVersion() + 1);
            return route;
        } else {
            return RouteEntity.builder()
                    .version(0)
                    .routeActions(singletonList(RouteActionEntity.builder()
                            .action(WAIT)
                            .marker(0)
                            .build()))
                    .build();
        }
    }
}
