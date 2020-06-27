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

import static de.tuberlin.sese.cabservice.persistence.job.JobEntity.CustomerState.*;
import static de.tuberlin.sese.cabservice.persistence.route.RouteActionEntity.Action.*;
import static java.util.Collections.emptyList;

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

        if (!cabRepo.findById(cabId).isPresent()) {
            throw new UnknownCabIdException("Cannot get route for unknown cab ID");
        }

        Optional<RouteEntity> routeOptional = routeRepo.findById(cabId);

        if (routeOptional.isPresent() && routeOptional.get().getJobId() != null) {
            RouteEntity route = routeOptional.get();
            Optional<JobEntity> jobOptional = jobService.getJob(route.getJobId());

            Preconditions.checkState(jobOptional.isPresent(), "Job should be present");
            Preconditions.checkState(jobOptional.get().isInProgress(), "Job should be in progress");

            JobEntity job = jobOptional.get();

            RouteEntity updatedRoute = buildRouteForJob(cabId, job);
            routeRepo.save(updatedRoute);

            if (updatedRoute.isSubRouteOf(route)) {
                return RouteEntity.builder()
                        .version(version)
                        .build();
            }

            updatedRoute.setVersion(updatedRoute.getVersion() + 1);
            routeRepo.save(updatedRoute);
            return updatedRoute;
        } else {
            // TODO Route should not be sent if nothing changed. In this else branch, a route could be available, but no job associated. Still, don't send whole route again.
            // TODO Change from Waiting at 0 -> job should already increment version
            // TODO Blocked: Routing should take into account markers which are not explicitly in route, but still part of route. If those are blocked, get new route.
            List<JobEntity> waitingJobs = jobService.getAllWaitingJobs();

            if (waitingJobs.isEmpty()) {
                RouteEntity route = getRouteToDepot(cabId);
                routeRepo.save(route);
                return route;
            } else {
                JobEntity job = waitingJobs.get(0);
                RouteEntity route = buildRouteForJob(cabId, job);
                routeRepo.save(route);
                job.setInProgress(true);
                jobService.updateJob(job);
                return route;
            }
        }
    }

    private RouteEntity buildRouteForJob(Long cabId, JobEntity job) {
        CabLocationEntity cabLocation = locationService.getCabLocation(cabId).orElseThrow(UnknownCabLocationException::new);

        List<RouteActionEntity> actions = new LinkedList<>();

        try {
            if (WAITING.equals(job.getCustomerState())) {
                actions.addAll(getActionsFromTo(cabLocation.getSection(), job.getStart()));

                actions.add(RouteActionEntity.builder()
                        .action(PICKUP)
                        .customerId(job.getCustomerId())
                        .marker(job.getStart())
                        .build());

                actions.addAll(getActionsFromTo(job.getStart(), job.getEnd()));

                actions.add(RouteActionEntity.builder()
                        .action(DROPOFF)
                        .customerId(job.getCustomerId())
                        .marker(job.getEnd())
                        .build());

                actions.addAll(getActionsFromTo(job.getEnd(), SECTION_DEPOT));

                actions.add(RouteActionEntity.builder()
                        .action(WAIT)
                        .marker(SECTION_DEPOT)
                        .build());
            }

            if (IN_CAB.equals(job.getCustomerState())) {
                actions.addAll(getActionsFromTo(cabLocation.getSection(), job.getEnd()));

                actions.add(RouteActionEntity.builder()
                        .action(DROPOFF)
                        .customerId(job.getCustomerId())
                        .marker(job.getEnd())
                        .build());

                actions.addAll(getActionsFromTo(job.getEnd(), SECTION_DEPOT));

                actions.add(RouteActionEntity.builder()
                        .action(WAIT)
                        .marker(SECTION_DEPOT)
                        .build());
            }

            if (AT_DESTINATION.equals(job.getCustomerState())) {
                actions.addAll(getActionsFromTo(cabLocation.getSection(), SECTION_DEPOT));

                actions.add(RouteActionEntity.builder()
                        .action(WAIT)
                        .marker(SECTION_DEPOT)
                        .build());
            }


        } catch (NoPathException e) {
            // TODO handle
        }

        return RouteEntity.builder()
                .version(0)
                .routeActions(actions)
                .cabId(cabId)
                .jobId(job.getId())
                .build();
    }

    private RouteEntity getRouteToDepot(Long cabId) {
        CabLocationEntity cabLocation = locationService.getCabLocation(cabId).orElseThrow(UnknownCabLocationException::new);

        List<RouteActionEntity> actions = new LinkedList<>();
        try {
            actions.addAll(getActionsFromTo(cabLocation.getSection(), SECTION_DEPOT));

            actions.add(RouteActionEntity.builder()
                    .action(WAIT)
                    .marker(SECTION_DEPOT)
                    .build());
        } catch (NoPathException e) {
            // TODO handle
        }

        return RouteEntity.builder()
                .version(0)
                .routeActions(actions)
                .cabId(cabId)
                .build();
    }

    private List<RouteActionEntity> getActionsFromTo(int from, int to) throws NoPathException {
        List<Option> routeOptions = pathFinder.getRouteBetween(from, to);

        if (routeOptions.size() == 0) {
            return emptyList();
        }

        List<RouteActionEntity> actions = new LinkedList<>();

        for (Option option : routeOptions) {
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

        return actions;
    }
}
