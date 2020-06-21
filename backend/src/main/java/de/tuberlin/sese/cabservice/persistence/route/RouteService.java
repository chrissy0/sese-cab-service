package de.tuberlin.sese.cabservice.persistence.route;

import com.google.common.base.Preconditions;
import de.tuberlin.sese.cabservice.logic.Option;
import de.tuberlin.sese.cabservice.logic.PathFinder;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationEntity;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationService;
import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.persistence.job.JobEntity;
import de.tuberlin.sese.cabservice.persistence.job.JobService;
import de.tuberlin.sese.cabservice.util.exceptions.NoPathException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabLocationException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static de.tuberlin.sese.cabservice.persistence.route.RouteActionEntity.Action.*;
import static java.util.Collections.singletonList;

@Service
@RequiredArgsConstructor
public class RouteService {

    private final RouteRepo routeRepo;

    private final CabRepo cabRepo;

    private final JobService jobService;

    private final CabLocationService locationService;

    private final PathFinder pathFinder;

    private static final Integer SECTION_DEPOT = 0;

    public RouteEntity getRoute(Long cabId, Integer version) {
        /*
          TODO
          Not finished
          - modularize buildRoute()
          - handle existing routed including version etc.
         */

        if (!cabRepo.findById(cabId).isPresent()) {
            throw new UnknownCabIdException("Cannot get route for unknown cab ID");
        }

        Optional<RouteEntity> routeOptional = routeRepo.findById(cabId);

        if (routeOptional.isPresent()) {
            // TODO if block is placeholder code
            RouteEntity route = routeOptional.get();
            route.setVersion(route.getVersion() + 1);
            return route;
        } else {
            List<JobEntity> waitingJobs = jobService.getAllWaitingJobs();

            if (waitingJobs.isEmpty()) {
                return RouteEntity.builder()
                        .version(0)
                        .routeActions(singletonList(RouteActionEntity.builder()
                                .action(WAIT)
                                .marker(0)
                                .build()))
                        .build();
            } else {
                JobEntity job = waitingJobs.get(0);
                return buildRoute(cabId, job);
            }
        }
    }

    private RouteEntity buildRoute(Long cabId, JobEntity job) {
        CabLocationEntity cabLocation = locationService.getCabLocation(cabId).orElseThrow(UnknownCabLocationException::new);

        List<Option> toStartOptions = null;
        List<Option> toEndOptions = null;
        List<Option> toDepotOptions = null;
        try {
            toStartOptions = pathFinder.getRouteBetween(cabLocation.getSection(), job.getStart());
            toEndOptions = pathFinder.getRouteBetween(job.getStart(), job.getEnd());
            toDepotOptions = pathFinder.getRouteBetween(job.getEnd(), SECTION_DEPOT);

        } catch (NoPathException e) {
            // TODO handle
        }

        if (toStartOptions == null || toEndOptions == null || toDepotOptions == null) {
            throw new IllegalStateException("Options are definitely not null, because getRouteBetween() never return null");
        }

        Preconditions.checkArgument(toEndOptions.size() > 0, "Start station must not equal end station");

        List<RouteActionEntity> actions = new LinkedList<>();

        for (Option option : toStartOptions) {
            if (Option.Direction.STRAIGHT.equals(option.getDirection())) {
                continue;
            }

            RouteActionEntity.Direction direction;
            if (Option.Direction.LEFT.equals(option.getDirection())) {
                direction = RouteActionEntity.Direction.LEFT;
            } else if (Option.Direction.RIGHT.equals(option.getDirection())) {
                direction = RouteActionEntity.Direction.RIGHT;
            } else {
                throw new IllegalStateException("Unknown direction");
            }

            actions.add(RouteActionEntity.builder()
                    .action(TURN)
                    .direction(direction)
                    .marker(option.getFromSection())
                    .build());
        }

        int startSection = toEndOptions.get(0).getFromSection();
        actions.add(RouteActionEntity.builder()
                .action(PICKUP)
                .customerId(job.getCustomerId())
                .marker(startSection)
                .build());

        for (Option option : toEndOptions) {
            if (Option.Direction.STRAIGHT.equals(option.getDirection())) {
                continue;
            }

            RouteActionEntity.Direction direction;
            if (Option.Direction.LEFT.equals(option.getDirection())) {
                direction = RouteActionEntity.Direction.LEFT;
            } else if (Option.Direction.RIGHT.equals(option.getDirection())) {
                direction = RouteActionEntity.Direction.RIGHT;
            } else {
                throw new IllegalStateException("Unknown direction");
            }

            actions.add(RouteActionEntity.builder()
                    .action(TURN)
                    .direction(direction)
                    .marker(option.getFromSection())
                    .build());
        }

        int endSection = toEndOptions.get(toEndOptions.size() - 1).getToSection();
        actions.add(RouteActionEntity.builder()
                .action(DROPOFF)
                .customerId(job.getCustomerId())
                .marker(endSection)
                .build());

        for (Option option : toDepotOptions) {
            if (Option.Direction.STRAIGHT.equals(option.getDirection())) {
                continue;
            }

            RouteActionEntity.Direction direction;
            if (Option.Direction.LEFT.equals(option.getDirection())) {
                direction = RouteActionEntity.Direction.LEFT;
            } else if (Option.Direction.RIGHT.equals(option.getDirection())) {
                direction = RouteActionEntity.Direction.RIGHT;
            } else {
                throw new IllegalStateException("Unknown direction");
            }

            actions.add(RouteActionEntity.builder()
                    .action(TURN)
                    .direction(direction)
                    .marker(option.getFromSection())
                    .build());
        }

        actions.add(RouteActionEntity.builder()
                .action(WAIT)
                .marker(SECTION_DEPOT)
                .build());

        return RouteEntity.builder()
                .version(0)
                .routeActions(actions)
                .build();
    }
}
