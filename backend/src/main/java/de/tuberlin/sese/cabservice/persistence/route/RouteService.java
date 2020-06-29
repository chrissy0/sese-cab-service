package de.tuberlin.sese.cabservice.persistence.route;

import de.tuberlin.sese.cabservice.logic.Option;
import de.tuberlin.sese.cabservice.logic.PathFinder;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationEntity;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationService;
import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.persistence.job.JobEntity;
import de.tuberlin.sese.cabservice.persistence.job.JobService;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabLocationException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static com.google.common.collect.Streams.stream;
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

    public RouteEntity getRoute(Long cabId, int version) {
        validateCabId(cabId);

        Optional<RouteEntity> routeOptional = routeRepo.findById(cabId);

        if (routeOptional.isPresent()) {
            RouteEntity loadedRoute = routeOptional.get();
            if (isJobRoute(loadedRoute)) {
                Optional<JobEntity> jobOptional = jobService.getJob(loadedRoute.getJobId());
                if (jobOptional.isPresent()) {
                    RouteEntity updatedRoute = buildRouteForJob(cabId, jobOptional.get(), loadedRoute.getVersion());
                    if (updatedRoute.isSubRouteOf(loadedRoute)) {
                        return getRouteToReturnForSubRoute(version, loadedRoute, updatedRoute);
                    }
                    return incrementVersion(updatedRoute);
                } else {
                    loadedRoute.setJobId(null);
                    routeRepo.save(loadedRoute);
                    return getRoute(cabId, version);
                }
            } else {
                Optional<JobEntity> jobOptional = getFirstAvailableJobAndSetInProgress();
                if (jobOptional.isPresent()) {
                    RouteEntity route = buildRouteForJob(cabId, jobOptional.get(), loadedRoute.getVersion() + 1);
                    routeRepo.save(route);
                    return route;
                } else {
                    RouteEntity updatedRoute = getRouteToDepot(cabId, loadedRoute.getVersion());
                    if (updatedRoute.isSubRouteOf(loadedRoute)) {
                        return getRouteToReturnForSubRoute(version, loadedRoute, updatedRoute);
                    }
                    return incrementVersion(updatedRoute);
                }
            }
        } else {
            Optional<JobEntity> jobOptional = getFirstAvailableJobAndSetInProgress();
            RouteEntity route;
            if (jobOptional.isPresent()) {
                route = buildRouteForJob(cabId, jobOptional.get(), 0);
            } else {
                route = getRouteToDepot(cabId, 0);
            }
            routeRepo.save(route);
            return route;
        }
    }

    public void removeJobFromRoutes(Long jobId) {
        validateJobId(jobId);

        List<RouteEntity> routesOfJob = stream(routeRepo.findAll())
                .filter(route -> jobId.equals(route.getJobId()))
                .collect(Collectors.toList());

        for (RouteEntity route : routesOfJob) {
            route.setJobId(null);
            routeRepo.save(route);
        }
    }

    private Optional<JobEntity> getFirstAvailableJobAndSetInProgress() {
        List<JobEntity> availableJobs = jobService.getAllWaitingJobs();
        if (availableJobs.isEmpty()) {
            return Optional.empty();
        }
        JobEntity job = availableJobs.get(0);
        job.setInProgress(true);
        jobService.updateJob(job);
        return Optional.of(job);
    }

    private RouteEntity incrementVersion(RouteEntity route) {
        route.setVersion(route.getVersion() + 1);
        routeRepo.save(route);
        return route;
    }

    private boolean isJobRoute(RouteEntity route) {
        return route.getJobId() != null;
    }

    private void validateCabId(Long cabId) {
        if (cabId == null) {
            throw new IllegalArgumentException("Cab ID was null");
        }
        if (!cabRepo.findById(cabId).isPresent()) {
            throw new UnknownCabIdException("Cannot get route for unknown cab ID");
        }
    }

    private void validateJobId(Long jobId) {
        if (jobId == null) {
            throw new IllegalArgumentException("Job ID was null");
        }
    }

    private RouteEntity getRouteToReturnForSubRoute(Integer version, RouteEntity loadedRoute, RouteEntity updatedRoute) {
        if (version.equals(loadedRoute.getVersion())) {
            return RouteEntity.builder()
                    .version(version)
                    .build();
        }
        return updatedRoute;
    }

    @SuppressWarnings("DuplicatedCode")
    private RouteEntity buildRouteForJob(Long cabId, JobEntity job, int version) {
        CabLocationEntity cabLocation = locationService.getCabLocation(cabId).orElseThrow(UnknownCabLocationException::new);

        List<RouteActionEntity> actions = new LinkedList<>();

        if (WAITING.equals(job.getCustomerState())) {
            actions.addAll(getActionsFromTo(cabLocation.getSection(), job.getStart()));

            if (routeIsFinished(actions)) {
                return RouteEntity.builder()
                        .version(version)
                        .routeActions(actions)
                        .cabId(cabId)
                        .jobId(job.getId())
                        .build();
            }

            actions.add(RouteActionEntity.builder()
                    .action(PICKUP)
                    .customerId(job.getCustomerId())
                    .marker(job.getStart())
                    .build());

            actions.addAll(getActionsFromTo(job.getStart(), job.getEnd()));

            if (routeIsFinished(actions)) {
                return RouteEntity.builder()
                        .version(version)
                        .routeActions(actions)
                        .cabId(cabId)
                        .jobId(job.getId())
                        .build();
            }

            actions.add(RouteActionEntity.builder()
                    .action(DROPOFF)
                    .customerId(job.getCustomerId())
                    .marker(job.getEnd())
                    .build());

            actions.addAll(getActionsFromTo(job.getEnd(), SECTION_DEPOT));

            if (routeIsFinished(actions)) {
                return RouteEntity.builder()
                        .version(version)
                        .routeActions(actions)
                        .cabId(cabId)
                        .jobId(job.getId())
                        .build();
            }

            actions.add(RouteActionEntity.builder()
                    .action(WAIT)
                    .marker(SECTION_DEPOT)
                    .build());
        }

        if (IN_CAB.equals(job.getCustomerState())) {
            actions.addAll(getActionsFromTo(cabLocation.getSection(), job.getEnd()));

            if (routeIsFinished(actions)) {
                return RouteEntity.builder()
                        .version(version)
                        .routeActions(actions)
                        .cabId(cabId)
                        .jobId(job.getId())
                        .build();
            }

            actions.add(RouteActionEntity.builder()
                    .action(DROPOFF)
                    .customerId(job.getCustomerId())
                    .marker(job.getEnd())
                    .build());

            actions.addAll(getActionsFromTo(job.getEnd(), SECTION_DEPOT));

            if (routeIsFinished(actions)) {
                return RouteEntity.builder()
                        .version(version)
                        .routeActions(actions)
                        .cabId(cabId)
                        .jobId(job.getId())
                        .build();
            }

            actions.add(RouteActionEntity.builder()
                    .action(WAIT)
                    .marker(SECTION_DEPOT)
                    .build());
        }

        if (AT_DESTINATION.equals(job.getCustomerState())) {
            actions.addAll(getActionsFromTo(cabLocation.getSection(), SECTION_DEPOT));

            if (routeIsFinished(actions)) {
                return RouteEntity.builder()
                        .version(version)
                        .routeActions(actions)
                        .cabId(cabId)
                        .jobId(job.getId())
                        .build();
            }

            actions.add(RouteActionEntity.builder()
                    .action(WAIT)
                    .marker(SECTION_DEPOT)
                    .build());
        }


        return RouteEntity.builder()
                .version(version)
                .routeActions(actions)
                .cabId(cabId)
                .jobId(job.getId())
                .build();
    }

    private boolean routeIsFinished(List<RouteActionEntity> actions) {
        if (actions.size() == 0) {
            return false;
        }
        return WAIT.equals(actions.get(actions.size() - 1).getAction());
    }

    private RouteEntity getRouteToDepot(Long cabId, int version) {
        CabLocationEntity cabLocation = locationService.getCabLocation(cabId).orElseThrow(UnknownCabLocationException::new);

        List<RouteActionEntity> actions = new LinkedList<>(getActionsFromTo(cabLocation.getSection(), SECTION_DEPOT));

        if (routeIsFinished(actions)) {
            return RouteEntity.builder()
                    .version(version)
                    .routeActions(actions)
                    .cabId(cabId)
                    .build();
        }

        actions.add(RouteActionEntity.builder()
                .action(WAIT)
                .marker(SECTION_DEPOT)
                .build());

        return RouteEntity.builder()
                .version(version)
                .routeActions(actions)
                .cabId(cabId)
                .build();
    }

    private List<RouteActionEntity> getActionsFromTo(int from, int to) {
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
            } else if (Option.Direction.WAIT.equals(option.getDirection())) {
                actions.add(RouteActionEntity.builder()
                        .action(WAIT)
                        .marker(option.getFromSection())
                        .build());
                return actions;
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
