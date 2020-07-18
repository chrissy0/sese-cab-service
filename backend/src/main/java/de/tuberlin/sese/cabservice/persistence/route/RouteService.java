package de.tuberlin.sese.cabservice.persistence.route;

import de.tuberlin.sese.cabservice.logic.Option;
import de.tuberlin.sese.cabservice.logic.PathFinder;
import de.tuberlin.sese.cabservice.persistence.cab.blocked.CabBlockedService;
import de.tuberlin.sese.cabservice.persistence.cab.dysfunctional.CabDysfunctionalService;
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

    private final CabBlockedService blockedService;

    private final CabDysfunctionalService dysfunctionalService;

    private final PathFinder pathFinder;

    private static final Integer SECTION_DEPOT = 0;

    /**
     * Returns the ideal route for a given cab.
     */
    public RouteEntity getRoute(Long cabId, int version) {
        validateCabId(cabId);

        Optional<RouteEntity> routeOptional = routeRepo.findById(cabId);

        if (routeOptional.isPresent()) {
            // Route exists in database
            RouteEntity loadedRoute = routeOptional.get();

            if (loadedRoute.getJobId() != null) {
                // Database route has job associated with it
                Optional<JobEntity> jobOptional = jobService.getJob(loadedRoute.getJobId());
                if (jobOptional.isPresent()) {
                    if (loadedRoute.getJobId2() != null) {
                        // if 2 jobs are set
                        Optional<JobEntity> jobOptional2 = jobService.getJob(loadedRoute.getJobId2());
                        if (jobOptional2.isPresent()) {
                            RouteEntity updatedRoute = buildRouteForJobs(cabId, jobOptional.get(), jobOptional2.get(), loadedRoute.getVersion());
                            if (updatedRoute.isSubRouteOf(loadedRoute)) {
                                routeRepo.save(updatedRoute);
                                return getRouteToReturnForSubRoute(version, loadedRoute, updatedRoute);
                            }
                            return incrementVersionAndSave(updatedRoute);
                        } else {
                            // if second job id is set but second job is not available
                            loadedRoute.setJobIds(loadedRoute.getJobId());
                            routeRepo.save(loadedRoute);
                            return getRoute(cabId, version);
                        }
                    } else {
                        // if only job 1 is set
                        RouteEntity updatedRoute = buildRouteForJobs(cabId, jobOptional.get(), null, loadedRoute.getVersion());
                        if (updatedRoute.isSubRouteOf(loadedRoute)) {
                            routeRepo.save(updatedRoute);
                            return getRouteToReturnForSubRoute(version, loadedRoute, updatedRoute);
                        }
                        return incrementVersionAndSave(updatedRoute);
                    }
                } else {
                    // if job id 1 is set, but associated job does not exist
                    if (loadedRoute.getJobId2() != null) {
                        loadedRoute.setJobIds(loadedRoute.getJobId2());
                    } else {
                        loadedRoute.setJobIds();
                    }
                    routeRepo.save(loadedRoute);
                    return getRoute(cabId, version);
                }
            } else {
                // Database route does not have job associated with it
                Optional<JobEntity> jobOptional1 = getFirstAvailableJob();
                if (jobOptional1.isPresent() && cabShouldGetNewJob(cabId)) {
                    // At least one job is available

                    Optional<JobEntity> jobOptional2 = getSecondAvailableJob();
                    if (jobOptional2.isPresent()) {
                        JobEntity job1 = jobOptional1.get();
                        setJobInProgress(job1);
                        JobEntity job2 = jobOptional2.get();
                        setJobInProgress(job2);
                        RouteEntity route = buildRouteForJobs(cabId, job1, job2, loadedRoute.getVersion() + 1);
                        routeRepo.save(route);
                        return route;
                    }

                    JobEntity job1 = jobOptional1.get();
                    setJobInProgress(job1);
                    RouteEntity route = buildRouteForJobs(cabId, job1, null, loadedRoute.getVersion() + 1);
                    routeRepo.save(route);
                    return route;
                } else {
                    // No job is available
                    RouteEntity updatedRoute = getRouteToDepot(cabId, loadedRoute.getVersion());
                    if (updatedRoute.isSubRouteOf(loadedRoute)) {
                        routeRepo.save(updatedRoute);
                        return getRouteToReturnForSubRoute(version, loadedRoute, updatedRoute);
                    }
                    return incrementVersionAndSave(updatedRoute);
                }
            }
        } else {
            // Route does not exist in database
            Optional<JobEntity> jobOptional1 = getFirstAvailableJob();

            if (jobOptional1.isPresent() && cabShouldGetNewJob(cabId)) {
                // At least one job is available

                Optional<JobEntity> jobOptional2 = getSecondAvailableJob();
                if (jobOptional2.isPresent()) {
                    JobEntity job1 = jobOptional1.get();
                    setJobInProgress(job1);
                    JobEntity job2 = jobOptional2.get();
                    setJobInProgress(job2);
                    RouteEntity route = buildRouteForJobs(cabId, job1, job2, 0);
                    routeRepo.save(route);
                    return route;
                }

                JobEntity job1 = jobOptional1.get();
                setJobInProgress(job1);
                RouteEntity route = buildRouteForJobs(cabId, job1, null, 0);
                routeRepo.save(route);
                return route;
            } else {
                // No job is available
                RouteEntity route = getRouteToDepot(cabId, 0);
                routeRepo.save(route);
                return route;
            }
        }
    }

    private void setJobInProgress(JobEntity job) {
        job.setInProgress(true);
        jobService.updateJob(job);
    }

    /**
     * Checks different conditions to determine whether or not a cab should be assigned a job
     */
    private boolean cabShouldGetNewJob(Long cabId) {
        if (blockedService.isBlocked(cabId)) {
            return false;
        }

        if (dysfunctionalService.isDysfunctional(cabId)) {
            return false;
        }

        Optional<CabLocationEntity> locationOptional = locationService.getCabLocation(cabId);
        if (locationOptional.isPresent() && locationOptional.get().getSection() != null) {
            int location = locationOptional.get().getSection();
            return location != 12 && location != 14 && location != 15;
        } else {
            throw new UnknownCabLocationException();
        }
    }

    /**
     * If a job is deleted, it should be removed from all routes.
     * Increments version so EC receives changes upon requesting route.
     */
    public void removeJobFromRoutes(Long jobId) {
        validateJobId(jobId);

        List<RouteEntity> routesOfJob = stream(routeRepo.findAll())
                .filter(route -> jobId.equals(route.getJobId()) || jobId.equals(route.getJobId2()))
                .collect(Collectors.toList());

        for (RouteEntity route : routesOfJob) {
            Long jobId1 = route.getJobId();
            Long jobId2 = route.getJobId2();

            if (jobId2 == null) {
                route.setJobIds();
            } else {
                if (jobId1.equals(jobId)) {
                    route.setJobIds(jobId2);
                } else if (jobId2.equals(jobId)) {
                    route.setJobIds(jobId1);
                }
            }

            // if for some reason the same job id was set in both job id fields
            if (jobId.equals(route.getJobId())) {
                route.setJobIds();
            }

            route.setVersion(route.getVersion() + 1);

            routeRepo.save(route);
        }
    }

    private Optional<JobEntity> getFirstAvailableJob() {
        List<JobEntity> availableJobs = jobService.getAllWaitingJobs();
        if (availableJobs.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(availableJobs.get(0));
    }

    private Optional<JobEntity> getSecondAvailableJob() {
        List<JobEntity> availableJobs = jobService.getAllWaitingJobs();
        if (availableJobs.size() < 2) {
            return Optional.empty();
        }
        return Optional.of(availableJobs.get(1));
    }

    private RouteEntity incrementVersionAndSave(RouteEntity route) {
        route.setVersion(route.getVersion() + 1);
        routeRepo.save(route);
        return route;
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

    /**
     * If updatedRoute is a subroute of loadedRoute, this method handles the return value.
     * If the version didn't change, there's no need to send the whole route to the EC again.
     */
    private RouteEntity getRouteToReturnForSubRoute(Integer version, RouteEntity loadedRoute, RouteEntity updatedRoute) {
        if (version.equals(loadedRoute.getVersion())) {
            return RouteEntity.builder()
                    .version(version)
                    .build();
        }
        return updatedRoute;
    }

    // TODO check manually and carefully

    /**
     * This method creates a route (RouteEntity) for a given cab and up to two jobs.
     * It takes into account the cab's current location.
     */
    @SuppressWarnings("DuplicatedCode")
    private RouteEntity buildRouteForJobs(Long cabId, JobEntity job1, JobEntity job2, int version) {
        if (job2 == null) {
            return buildRouteForJob(cabId, job1, version);
        }
        if (AT_DESTINATION.equals(job1.getCustomerState()) && AT_DESTINATION.equals(job2.getCustomerState())) {
            return getRouteToDepot(cabId, version);
        }
        if (AT_DESTINATION.equals(job1.getCustomerState())) {
            return buildRouteForJob(cabId, job2, version);
        }
        if (AT_DESTINATION.equals(job2.getCustomerState())) {
            return buildRouteForJob(cabId, job1, version);
        }

        CabLocationEntity cabLocation = locationService.getCabLocation(cabId).orElseThrow(UnknownCabLocationException::new);

        List<RouteActionEntity> actions = new LinkedList<>();

        // Combines multiple parts of a route into one list of actions, which is then added to a new route.
        // We have deliberately chosen to implement all possibilities manually instead of leveraging loops to
        // prevent bugs.

        if (WAITING.equals(job1.getCustomerState()) && WAITING.equals(job2.getCustomerState())) {
            List<RouteActionEntity> toJob1Start = getActionsFromTo(cabLocation.getSection(), job1.getStart());
            List<RouteActionEntity> toJob2Start = getActionsFromTo(cabLocation.getSection(), job2.getStart());

            if (toJob1Start.size() <= toJob2Start.size()) {
                // starting job 1
                actions.addAll(toJob1Start);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(PICKUP)
                        .customerId(job1.getCustomerId())
                        .marker(job1.getStart())
                        .build());

                toJob2Start = getActionsFromTo(job1.getStart(), job2.getStart());
                List<RouteActionEntity> toJob1End = getActionsFromTo(job1.getStart(), job1.getEnd());

                if (toJob2Start.size() < toJob1End.size()) {
                    // starting job 2
                    actions.addAll(toJob2Start);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(PICKUP)
                            .customerId(job2.getCustomerId())
                            .marker(job2.getStart())
                            .build());

                    toJob1End = getActionsFromTo(job2.getStart(), job1.getEnd());
                    List<RouteActionEntity> toJob2End = getActionsFromTo(job2.getStart(), job2.getEnd());

                    if (toJob1End.size() < toJob2End.size()) {
                        // ending job 1
                        actions.addAll(toJob1End);

                        if (routeIsFinished(actions)) {
                            return RouteEntity.builder()
                                    .version(version)
                                    .routeActions(actions)
                                    .cabId(cabId)
                                    .build()
                                    .setJobIds(job1.getId(), job2.getId());
                        }

                        actions.add(RouteActionEntity.builder()
                                .action(DROPOFF)
                                .customerId(job1.getCustomerId())
                                .marker(job1.getEnd())
                                .build());

                        // ending job 2
                        toJob2End = getActionsFromTo(job1.getEnd(), job2.getEnd());
                        actions.addAll(toJob2End);

                        if (routeIsFinished(actions)) {
                            return RouteEntity.builder()
                                    .version(version)
                                    .routeActions(actions)
                                    .cabId(cabId)
                                    .build()
                                    .setJobIds(job1.getId(), job2.getId());
                        }

                        actions.add(RouteActionEntity.builder()
                                .action(DROPOFF)
                                .customerId(job2.getCustomerId())
                                .marker(job2.getEnd())
                                .build());

                        // to depot
                        List<RouteActionEntity> toDepot = getActionsFromTo(job2.getEnd(), SECTION_DEPOT);
                        actions.addAll(toDepot);

                        if (routeIsFinished(actions)) {
                            return RouteEntity.builder()
                                    .version(version)
                                    .routeActions(actions)
                                    .cabId(cabId)
                                    .build()
                                    .setJobIds(job1.getId(), job2.getId());
                        }
                    } else {
                        // ending job 2
                        actions.addAll(toJob2End);

                        if (routeIsFinished(actions)) {
                            return RouteEntity.builder()
                                    .version(version)
                                    .routeActions(actions)
                                    .cabId(cabId)
                                    .build()
                                    .setJobIds(job1.getId(), job2.getId());
                        }

                        actions.add(RouteActionEntity.builder()
                                .action(DROPOFF)
                                .customerId(job2.getCustomerId())
                                .marker(job2.getEnd())
                                .build());

                        // ending job 1
                        toJob1End = getActionsFromTo(job2.getEnd(), job1.getEnd());
                        actions.addAll(toJob1End);

                        if (routeIsFinished(actions)) {
                            return RouteEntity.builder()
                                    .version(version)
                                    .routeActions(actions)
                                    .cabId(cabId)
                                    .build()
                                    .setJobIds(job1.getId(), job2.getId());
                        }

                        actions.add(RouteActionEntity.builder()
                                .action(DROPOFF)
                                .customerId(job1.getCustomerId())
                                .marker(job1.getEnd())
                                .build());

                        // to depot
                        List<RouteActionEntity> toDepot = getActionsFromTo(job1.getEnd(), SECTION_DEPOT);
                        actions.addAll(toDepot);

                        if (routeIsFinished(actions)) {
                            return RouteEntity.builder()
                                    .version(version)
                                    .routeActions(actions)
                                    .cabId(cabId)
                                    .build()
                                    .setJobIds(job1.getId(), job2.getId());
                        }
                    }
                } else {
                    // ending job 1
                    actions.addAll(toJob1End);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(DROPOFF)
                            .customerId(job1.getCustomerId())
                            .marker(job1.getEnd())
                            .build());

                    // starting job 2
                    toJob2Start = getActionsFromTo(job1.getEnd(), job2.getStart());
                    actions.addAll(toJob2Start);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(PICKUP)
                            .customerId(job2.getCustomerId())
                            .marker(job2.getStart())
                            .build());

                    // ending job 2
                    List<RouteActionEntity> toJob2End = getActionsFromTo(job2.getStart(), job2.getEnd());
                    actions.addAll(toJob2End);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(DROPOFF)
                            .customerId(job2.getCustomerId())
                            .marker(job2.getEnd())
                            .build());

                    // to depot
                    List<RouteActionEntity> toDepot = getActionsFromTo(job2.getEnd(), SECTION_DEPOT);
                    actions.addAll(toDepot);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }
                }
            } else {
                // starting job 2
                actions.addAll(toJob2Start);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(PICKUP)
                        .customerId(job2.getCustomerId())
                        .marker(job2.getStart())
                        .build());

                toJob1Start = getActionsFromTo(job2.getStart(), job1.getStart());
                List<RouteActionEntity> toJob2End = getActionsFromTo(job2.getStart(), job2.getEnd());

                if (toJob1Start.size() < toJob2End.size()) {
                    // starting job 1
                    actions.addAll(toJob1Start);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(PICKUP)
                            .customerId(job1.getCustomerId())
                            .marker(job1.getStart())
                            .build());

                    List<RouteActionEntity> toJob1End = getActionsFromTo(job1.getStart(), job1.getEnd());
                    toJob2End = getActionsFromTo(job1.getStart(), job2.getEnd());

                    if (toJob1End.size() < toJob2End.size()) {
                        // ending job 1
                        actions.addAll(toJob1End);

                        if (routeIsFinished(actions)) {
                            return RouteEntity.builder()
                                    .version(version)
                                    .routeActions(actions)
                                    .cabId(cabId)
                                    .build()
                                    .setJobIds(job1.getId(), job2.getId());
                        }

                        actions.add(RouteActionEntity.builder()
                                .action(DROPOFF)
                                .customerId(job1.getCustomerId())
                                .marker(job1.getEnd())
                                .build());

                        // ending job 2
                        toJob2End = getActionsFromTo(job1.getEnd(), job2.getEnd());
                        actions.addAll(toJob2End);

                        if (routeIsFinished(actions)) {
                            return RouteEntity.builder()
                                    .version(version)
                                    .routeActions(actions)
                                    .cabId(cabId)
                                    .build()
                                    .setJobIds(job1.getId(), job2.getId());
                        }

                        actions.add(RouteActionEntity.builder()
                                .action(DROPOFF)
                                .customerId(job2.getCustomerId())
                                .marker(job2.getEnd())
                                .build());

                        // to depot
                        List<RouteActionEntity> toDepot = getActionsFromTo(job2.getEnd(), SECTION_DEPOT);
                        actions.addAll(toDepot);

                        if (routeIsFinished(actions)) {
                            return RouteEntity.builder()
                                    .version(version)
                                    .routeActions(actions)
                                    .cabId(cabId)
                                    .build()
                                    .setJobIds(job1.getId(), job2.getId());
                        }
                    } else {
                        // ending job 2
                        actions.addAll(toJob2End);

                        if (routeIsFinished(actions)) {
                            return RouteEntity.builder()
                                    .version(version)
                                    .routeActions(actions)
                                    .cabId(cabId)
                                    .build()
                                    .setJobIds(job1.getId(), job2.getId());
                        }

                        actions.add(RouteActionEntity.builder()
                                .action(DROPOFF)
                                .customerId(job2.getCustomerId())
                                .marker(job2.getEnd())
                                .build());

                        // ending job 1
                        toJob1End = getActionsFromTo(job2.getEnd(), job1.getEnd());
                        actions.addAll(toJob1End);

                        if (routeIsFinished(actions)) {
                            return RouteEntity.builder()
                                    .version(version)
                                    .routeActions(actions)
                                    .cabId(cabId)
                                    .build()
                                    .setJobIds(job1.getId(), job2.getId());
                        }

                        actions.add(RouteActionEntity.builder()
                                .action(DROPOFF)
                                .customerId(job1.getCustomerId())
                                .marker(job1.getEnd())
                                .build());

                        // to depot
                        List<RouteActionEntity> toDepot = getActionsFromTo(job1.getEnd(), SECTION_DEPOT);
                        actions.addAll(toDepot);

                        if (routeIsFinished(actions)) {
                            return RouteEntity.builder()
                                    .version(version)
                                    .routeActions(actions)
                                    .cabId(cabId)
                                    .build()
                                    .setJobIds(job1.getId(), job2.getId());
                        }
                    }
                } else {
                    // ending job 2
                    actions.addAll(toJob2End);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(DROPOFF)
                            .customerId(job2.getCustomerId())
                            .marker(job2.getEnd())
                            .build());

                    // starting job 1
                    toJob1Start = getActionsFromTo(job2.getEnd(), job1.getStart());
                    actions.addAll(toJob1Start);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(PICKUP)
                            .customerId(job1.getCustomerId())
                            .marker(job1.getStart())
                            .build());

                    // ending job 1
                    List<RouteActionEntity> toJob1End = getActionsFromTo(job1.getStart(), job1.getEnd());
                    actions.addAll(toJob1End);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(DROPOFF)
                            .customerId(job1.getCustomerId())
                            .marker(job1.getEnd())
                            .build());

                    // to depot
                    List<RouteActionEntity> toDepot = getActionsFromTo(job1.getEnd(), SECTION_DEPOT);
                    actions.addAll(toDepot);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }
                }
            }
        } else if (IN_CAB.equals(job1.getCustomerState()) && IN_CAB.equals(job2.getCustomerState())) {
            List<RouteActionEntity> toJob1End = getActionsFromTo(cabLocation.getSection(), job1.getEnd());
            List<RouteActionEntity> toJob2End = getActionsFromTo(cabLocation.getSection(), job2.getEnd());

            if (toJob1End.size() < toJob2End.size()) {
                // ending job 1
                actions.addAll(toJob1End);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(DROPOFF)
                        .customerId(job1.getCustomerId())
                        .marker(job1.getEnd())
                        .build());

                //ending job 2
                toJob2End = getActionsFromTo(job1.getEnd(), job2.getEnd());
                actions.addAll(toJob2End);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(DROPOFF)
                        .customerId(job2.getCustomerId())
                        .marker(job2.getEnd())
                        .build());

                // to depot
                List<RouteActionEntity> toDepot = getActionsFromTo(job2.getEnd(), SECTION_DEPOT);
                actions.addAll(toDepot);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }
            } else {
                // ending job 2
                actions.addAll(toJob2End);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(DROPOFF)
                        .customerId(job2.getCustomerId())
                        .marker(job2.getEnd())
                        .build());

                //ending job 1
                toJob1End = getActionsFromTo(job2.getEnd(), job1.getEnd());
                actions.addAll(toJob1End);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(DROPOFF)
                        .customerId(job1.getCustomerId())
                        .marker(job1.getEnd())
                        .build());

                // to depot
                List<RouteActionEntity> toDepot = getActionsFromTo(job1.getEnd(), SECTION_DEPOT);
                actions.addAll(toDepot);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }
            }
        } else if (WAITING.equals(job1.getCustomerState()) && IN_CAB.equals(job2.getCustomerState())) {
            List<RouteActionEntity> toJob1Start = getActionsFromTo(cabLocation.getSection(), job1.getStart());
            List<RouteActionEntity> toJob2End = getActionsFromTo(cabLocation.getSection(), job2.getEnd());

            if (toJob1Start.size() < toJob2End.size()) {
                // starting job 1
                actions.addAll(toJob1Start);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(PICKUP)
                        .customerId(job1.getCustomerId())
                        .marker(job1.getStart())
                        .build());

                List<RouteActionEntity> toJob1End = getActionsFromTo(job1.getStart(), job1.getEnd());
                toJob2End = getActionsFromTo(job1.getStart(), job2.getEnd());

                if (toJob1End.size() < toJob2End.size()) {
                    // ending job 1
                    actions.addAll(toJob1End);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(DROPOFF)
                            .customerId(job1.getCustomerId())
                            .marker(job1.getEnd())
                            .build());

                    // ending job 2
                    toJob2End = getActionsFromTo(job1.getEnd(), job2.getEnd());
                    actions.addAll(toJob2End);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(DROPOFF)
                            .customerId(job2.getCustomerId())
                            .marker(job2.getEnd())
                            .build());

                    // to depot
                    List<RouteActionEntity> toDepot = getActionsFromTo(job2.getEnd(), SECTION_DEPOT);
                    actions.addAll(toDepot);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }
                } else {
                    // ending job 2
                    actions.addAll(toJob2End);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(DROPOFF)
                            .customerId(job2.getCustomerId())
                            .marker(job2.getEnd())
                            .build());

                    // ending job 1
                    toJob1End = getActionsFromTo(job2.getEnd(), job1.getEnd());
                    actions.addAll(toJob1End);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(DROPOFF)
                            .customerId(job1.getCustomerId())
                            .marker(job1.getEnd())
                            .build());

                    // to depot
                    List<RouteActionEntity> toDepot = getActionsFromTo(job1.getEnd(), SECTION_DEPOT);
                    actions.addAll(toDepot);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }
                }
            } else {
                // ending job 2
                actions.addAll(toJob2End);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(DROPOFF)
                        .customerId(job2.getCustomerId())
                        .marker(job2.getEnd())
                        .build());

                // starting job 1
                toJob1Start = getActionsFromTo(job2.getEnd(), job1.getStart());
                actions.addAll(toJob1Start);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(PICKUP)
                        .customerId(job1.getCustomerId())
                        .marker(job1.getStart())
                        .build());

                // ending job 1
                List<RouteActionEntity> toJob1End = getActionsFromTo(job1.getStart(), job1.getEnd());
                actions.addAll(toJob1End);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(DROPOFF)
                        .customerId(job1.getCustomerId())
                        .marker(job1.getEnd())
                        .build());

                // to depot
                List<RouteActionEntity> toDepot = getActionsFromTo(job1.getEnd(), SECTION_DEPOT);
                actions.addAll(toDepot);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }
            }
        } else if (IN_CAB.equals(job1.getCustomerState()) && WAITING.equals(job2.getCustomerState())) {
            List<RouteActionEntity> toJob1End = getActionsFromTo(cabLocation.getSection(), job1.getEnd());
            List<RouteActionEntity> toJob2Start = getActionsFromTo(cabLocation.getSection(), job2.getStart());

            if (toJob2Start.size() < toJob1End.size()) {
                // starting job 2
                actions.addAll(toJob2Start);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(PICKUP)
                        .customerId(job2.getCustomerId())
                        .marker(job2.getStart())
                        .build());

                toJob1End = getActionsFromTo(job2.getStart(), job1.getEnd());
                List<RouteActionEntity> toJob2End = getActionsFromTo(job2.getStart(), job2.getEnd());

                if (toJob1End.size() < toJob2End.size()) {
                    // ending job 1
                    actions.addAll(toJob1End);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(DROPOFF)
                            .customerId(job1.getCustomerId())
                            .marker(job1.getEnd())
                            .build());

                    // ending job 2
                    toJob2End = getActionsFromTo(job1.getEnd(), job2.getEnd());
                    actions.addAll(toJob2End);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(DROPOFF)
                            .customerId(job2.getCustomerId())
                            .marker(job2.getEnd())
                            .build());

                    // to depot
                    List<RouteActionEntity> toDepot = getActionsFromTo(job2.getEnd(), SECTION_DEPOT);
                    actions.addAll(toDepot);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }
                } else {
                    // ending job 2
                    actions.addAll(toJob2End);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(DROPOFF)
                            .customerId(job2.getCustomerId())
                            .marker(job2.getEnd())
                            .build());

                    // ending job 1
                    toJob1End = getActionsFromTo(job2.getEnd(), job1.getEnd());
                    actions.addAll(toJob1End);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }

                    actions.add(RouteActionEntity.builder()
                            .action(DROPOFF)
                            .customerId(job1.getCustomerId())
                            .marker(job1.getEnd())
                            .build());

                    // to depot
                    List<RouteActionEntity> toDepot = getActionsFromTo(job1.getEnd(), SECTION_DEPOT);
                    actions.addAll(toDepot);

                    if (routeIsFinished(actions)) {
                        return RouteEntity.builder()
                                .version(version)
                                .routeActions(actions)
                                .cabId(cabId)
                                .build()
                                .setJobIds(job1.getId(), job2.getId());
                    }
                }
            } else {
                // ending job 1
                actions.addAll(toJob1End);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(DROPOFF)
                        .customerId(job1.getCustomerId())
                        .marker(job1.getEnd())
                        .build());

                // starting job 2
                toJob2Start = getActionsFromTo(job1.getEnd(), job2.getStart());
                actions.addAll(toJob2Start);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(PICKUP)
                        .customerId(job2.getCustomerId())
                        .marker(job2.getStart())
                        .build());

                // ending job 2
                List<RouteActionEntity> toJob2End = getActionsFromTo(job2.getStart(), job2.getEnd());
                actions.addAll(toJob2End);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }

                actions.add(RouteActionEntity.builder()
                        .action(DROPOFF)
                        .customerId(job2.getCustomerId())
                        .marker(job2.getEnd())
                        .build());

                // to depot
                List<RouteActionEntity> toDepot = getActionsFromTo(job2.getEnd(), SECTION_DEPOT);
                actions.addAll(toDepot);

                if (routeIsFinished(actions)) {
                    return RouteEntity.builder()
                            .version(version)
                            .routeActions(actions)
                            .cabId(cabId)
                            .build()
                            .setJobIds(job1.getId(), job2.getId());
                }
            }
        }

        actions.add(RouteActionEntity.builder()
                .action(WAIT)
                .marker(SECTION_DEPOT)
                .build());


        return RouteEntity.builder()
                .version(version)
                .routeActions(actions)
                .cabId(cabId)
                .build()
                .setJobIds(job1.getId(), job2.getId());
    }

    /**
     * Creates a route for a single job
     */
    @SuppressWarnings("DuplicatedCode")
    private RouteEntity buildRouteForJob(Long cabId, JobEntity job1, int version) {
        CabLocationEntity cabLocation = locationService.getCabLocation(cabId).orElseThrow(UnknownCabLocationException::new);

        List<RouteActionEntity> actions = new LinkedList<>();

        if (WAITING.equals(job1.getCustomerState())) {
            actions.addAll(getActionsFromTo(cabLocation.getSection(), job1.getStart()));

            if (routeIsFinished(actions)) {
                return RouteEntity.builder()
                        .version(version)
                        .routeActions(actions)
                        .cabId(cabId)
                        .build()
                        .setJobIds(job1.getId());
            }

            actions.add(RouteActionEntity.builder()
                    .action(PICKUP)
                    .customerId(job1.getCustomerId())
                    .marker(job1.getStart())
                    .build());

            actions.addAll(getActionsFromTo(job1.getStart(), job1.getEnd()));

            if (routeIsFinished(actions)) {
                return RouteEntity.builder()
                        .version(version)
                        .routeActions(actions)
                        .cabId(cabId)
                        .build()
                        .setJobIds(job1.getId());
            }

            actions.add(RouteActionEntity.builder()
                    .action(DROPOFF)
                    .customerId(job1.getCustomerId())
                    .marker(job1.getEnd())
                    .build());

            actions.addAll(getActionsFromTo(job1.getEnd(), SECTION_DEPOT));

            if (routeIsFinished(actions)) {
                return RouteEntity.builder()
                        .version(version)
                        .routeActions(actions)
                        .cabId(cabId)
                        .build()
                        .setJobIds(job1.getId());
            }

            actions.add(RouteActionEntity.builder()
                    .action(WAIT)
                    .marker(SECTION_DEPOT)
                    .build());
        }

        if (IN_CAB.equals(job1.getCustomerState())) {
            actions.addAll(getActionsFromTo(cabLocation.getSection(), job1.getEnd()));

            if (routeIsFinished(actions)) {
                return RouteEntity.builder()
                        .version(version)
                        .routeActions(actions)
                        .cabId(cabId)
                        .build()
                        .setJobIds(job1.getId());
            }

            actions.add(RouteActionEntity.builder()
                    .action(DROPOFF)
                    .customerId(job1.getCustomerId())
                    .marker(job1.getEnd())
                    .build());

            actions.addAll(getActionsFromTo(job1.getEnd(), SECTION_DEPOT));

            if (routeIsFinished(actions)) {
                return RouteEntity.builder()
                        .version(version)
                        .routeActions(actions)
                        .cabId(cabId)
                        .build()
                        .setJobIds(job1.getId());
            }

            actions.add(RouteActionEntity.builder()
                    .action(WAIT)
                    .marker(SECTION_DEPOT)
                    .build());
        }

        if (AT_DESTINATION.equals(job1.getCustomerState())) {
            actions.addAll(getActionsFromTo(cabLocation.getSection(), SECTION_DEPOT));

            if (routeIsFinished(actions)) {
                return RouteEntity.builder()
                        .version(version)
                        .routeActions(actions)
                        .cabId(cabId)
                        .build()
                        .setJobIds(job1.getId());
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
                .build()
                .setJobIds(job1.getId());
    }

    /**
     * Checks if a route ends on "WAIT"
     */
    private boolean routeIsFinished(List<RouteActionEntity> actions) {
        if (actions.size() == 0) {
            return false;
        }
        return WAIT.equals(actions.get(actions.size() - 1).getAction());
    }

    /**
     * Returns a route from the current cab location to the depot
     */
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

    /**
     * Leverages the PathFinder to return the actions (RouteActionEntity) required to get from one section to another
     */
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

    /**
     * Returns all routes in the routeRepo
     */
    public List<RouteEntity> getRoutes() {
        List<RouteEntity> entities = new LinkedList<>();
        routeRepo.findAll().forEach(entities::add);
        return entities;
    }
}
