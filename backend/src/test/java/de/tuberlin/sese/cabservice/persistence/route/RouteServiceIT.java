package de.tuberlin.sese.cabservice.persistence.route;

import de.tuberlin.sese.cabservice.persistence.cab.blocked.CabBlockedService;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationEntity;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationService;
import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRegistrationService;
import de.tuberlin.sese.cabservice.persistence.cab.registration.persistence.CabEntity;
import de.tuberlin.sese.cabservice.persistence.customerinteraction.dropoff.DropoffService;
import de.tuberlin.sese.cabservice.persistence.customerinteraction.pickup.PickupService;
import de.tuberlin.sese.cabservice.persistence.job.JobEntity;
import de.tuberlin.sese.cabservice.persistence.job.JobService;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.junit4.SpringRunner;

import static de.tuberlin.sese.cabservice.persistence.route.RouteActionEntity.Action.*;
import static de.tuberlin.sese.cabservice.persistence.route.RouteActionEntity.Direction.LEFT;
import static de.tuberlin.sese.cabservice.persistence.route.RouteActionEntity.Direction.RIGHT;
import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.annotation.DirtiesContext.ClassMode.BEFORE_EACH_TEST_METHOD;

@RunWith(SpringRunner.class)
@SpringBootTest
@DirtiesContext(classMode = BEFORE_EACH_TEST_METHOD)
public class RouteServiceIT {

    @Autowired
    private RouteService routeService;

    @Autowired
    private CabRegistrationService registrationService;

    @Autowired
    private CabLocationService locationService;

    @Autowired
    private CabBlockedService blockedService;

    @Autowired
    private PickupService pickupService;

    @Autowired
    private DropoffService dropoffService;

    @Autowired
    private JobService jobService;

    @Test
    public void shouldReturnRouteForWaitingJob() {
        long cabId = registrationService.registerCab(CabEntity.builder()
                        .name("Some Cab Name")
                        .build(),
                0);


        long jobId = jobService.saveNewJob(JobEntity.builder()
                .start(2)
                .end(8)
                .build());

        RouteEntity route = routeService.getRoute(cabId, 0);

        assertThat(route.getVersion()).isEqualTo(0);
        assertThat(route.getCabId()).isEqualTo(cabId);
        assertThat(route.getJobId()).isEqualTo(jobId);

        assertThat(route.getRouteActions()).hasSize(9);

        assertThat(route.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(0).getDirection()).isEqualTo(RIGHT);
        assertThat(route.getRouteActions().get(0).getMarker()).isEqualTo(1);

        assertThat(route.getRouteActions().get(1).getAction()).isEqualTo(PICKUP);
        assertThat(route.getRouteActions().get(1).getCustomerId()).isEqualTo(jobId);
        assertThat(route.getRouteActions().get(1).getMarker()).isEqualTo(2);

        assertThat(route.getRouteActions().get(2).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(2).getDirection()).isEqualTo(LEFT);
        assertThat(route.getRouteActions().get(2).getMarker()).isEqualTo(4);

        assertThat(route.getRouteActions().get(3).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(3).getDirection()).isEqualTo(RIGHT);
        assertThat(route.getRouteActions().get(3).getMarker()).isEqualTo(7);

        assertThat(route.getRouteActions().get(4).getAction()).isEqualTo(DROPOFF);
        assertThat(route.getRouteActions().get(4).getCustomerId()).isEqualTo(jobId);
        assertThat(route.getRouteActions().get(4).getMarker()).isEqualTo(8);

        assertThat(route.getRouteActions().get(5).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(5).getDirection()).isEqualTo(LEFT);
        assertThat(route.getRouteActions().get(5).getMarker()).isEqualTo(10);

        assertThat(route.getRouteActions().get(6).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(6).getDirection()).isEqualTo(LEFT);
        assertThat(route.getRouteActions().get(6).getMarker()).isEqualTo(12);

        assertThat(route.getRouteActions().get(7).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(7).getDirection()).isEqualTo(RIGHT);
        assertThat(route.getRouteActions().get(7).getMarker()).isEqualTo(14);

        assertThat(route.getRouteActions().get(8).getAction()).isEqualTo(WAIT);
        assertThat(route.getRouteActions().get(8).getMarker()).isEqualTo(0);
    }

    @Test
    public void shouldReturnRouteForNoJobWaitingCabAtDepot() {
        long cabId = registrationService.registerCab(CabEntity.builder()
                        .name("Some Cab Name")
                        .build(),
                0);

        RouteEntity route = routeService.getRoute(cabId, 0);

        assertThat(route.getVersion()).isEqualTo(0);
        assertThat(route.getCabId()).isEqualTo(cabId);
        assertThat(route.getJobId()).isNull();

        assertThat(route.getRouteActions()).hasSize(1);

        assertThat(route.getRouteActions().get(0).getAction()).isEqualTo(WAIT);
        assertThat(route.getRouteActions().get(0).getMarker()).isEqualTo(0);
    }

    @Test
    public void shouldReturnUpdatedRouteForInProgressJobWhenRouteIsBlocked() {
        long cabId = registrationService.registerCab(CabEntity.builder()
                        .name("Some Cab Name")
                        .build(),
                0);


        long jobId = jobService.saveNewJob(JobEntity.builder()
                .start(2)
                .end(8)
                .build());

        RouteEntity route = routeService.getRoute(cabId, 0);

        assertThat(route.getVersion()).isEqualTo(0);
        assertThat(route.getCabId()).isEqualTo(cabId);
        assertThat(route.getJobId()).isEqualTo(jobId);

        assertThat(route.getRouteActions()).hasSize(9);

        assertThat(route.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(0).getDirection()).isEqualTo(RIGHT);
        assertThat(route.getRouteActions().get(0).getMarker()).isEqualTo(1);

        assertThat(route.getRouteActions().get(1).getAction()).isEqualTo(PICKUP);
        assertThat(route.getRouteActions().get(1).getCustomerId()).isEqualTo(jobId);
        assertThat(route.getRouteActions().get(1).getMarker()).isEqualTo(2);

        assertThat(route.getRouteActions().get(2).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(2).getDirection()).isEqualTo(LEFT);
        assertThat(route.getRouteActions().get(2).getMarker()).isEqualTo(4);

        assertThat(route.getRouteActions().get(3).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(3).getDirection()).isEqualTo(RIGHT);
        assertThat(route.getRouteActions().get(3).getMarker()).isEqualTo(7);

        assertThat(route.getRouteActions().get(4).getAction()).isEqualTo(DROPOFF);
        assertThat(route.getRouteActions().get(4).getCustomerId()).isEqualTo(jobId);
        assertThat(route.getRouteActions().get(4).getMarker()).isEqualTo(8);

        assertThat(route.getRouteActions().get(5).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(5).getDirection()).isEqualTo(LEFT);
        assertThat(route.getRouteActions().get(5).getMarker()).isEqualTo(10);

        assertThat(route.getRouteActions().get(6).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(6).getDirection()).isEqualTo(LEFT);
        assertThat(route.getRouteActions().get(6).getMarker()).isEqualTo(12);

        assertThat(route.getRouteActions().get(7).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(7).getDirection()).isEqualTo(RIGHT);
        assertThat(route.getRouteActions().get(7).getMarker()).isEqualTo(14);

        assertThat(route.getRouteActions().get(8).getAction()).isEqualTo(WAIT);
        assertThat(route.getRouteActions().get(8).getMarker()).isEqualTo(0);

        long blockedCabId = registrationService.registerCab(CabEntity.builder()
                        .name("Some Blocked Cab Name")
                        .build(),
                6);
        blockedService.setBlocked(blockedCabId, true);

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(2)
                .build());

        pickupService.pickup(cabId, jobId);

        pickupService.acceptPickup(jobId);

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(4)
                .build());

        RouteEntity updatedRoute = routeService.getRoute(cabId, 0);

        assertThat(updatedRoute.getVersion()).isEqualTo(1);
        assertThat(updatedRoute.getCabId()).isEqualTo(cabId);
        assertThat(updatedRoute.getJobId()).isEqualTo(jobId);

        assertThat(updatedRoute.getRouteActions()).hasSize(7);

        assertThat(updatedRoute.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(0).getDirection()).isEqualTo(RIGHT);
        assertThat(updatedRoute.getRouteActions().get(0).getMarker()).isEqualTo(4);

        assertThat(updatedRoute.getRouteActions().get(1).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(1).getDirection()).isEqualTo(RIGHT);
        assertThat(updatedRoute.getRouteActions().get(1).getMarker()).isEqualTo(7);

        assertThat(updatedRoute.getRouteActions().get(2).getAction()).isEqualTo(DROPOFF);
        assertThat(updatedRoute.getRouteActions().get(2).getCustomerId()).isEqualTo(jobId);
        assertThat(updatedRoute.getRouteActions().get(2).getMarker()).isEqualTo(8);

        assertThat(updatedRoute.getRouteActions().get(3).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(3).getDirection()).isEqualTo(LEFT);
        assertThat(updatedRoute.getRouteActions().get(3).getMarker()).isEqualTo(10);

        assertThat(updatedRoute.getRouteActions().get(4).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(4).getDirection()).isEqualTo(LEFT);
        assertThat(updatedRoute.getRouteActions().get(4).getMarker()).isEqualTo(12);

        assertThat(updatedRoute.getRouteActions().get(5).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(5).getDirection()).isEqualTo(RIGHT);
        assertThat(updatedRoute.getRouteActions().get(5).getMarker()).isEqualTo(14);

        assertThat(updatedRoute.getRouteActions().get(6).getAction()).isEqualTo(WAIT);
        assertThat(updatedRoute.getRouteActions().get(6).getMarker()).isEqualTo(0);
    }

    @Test
    public void shouldReturnUpdatedRouteForInProgressJobWhenRouteIsNotBlocked() {
        long cabId = registrationService.registerCab(CabEntity.builder()
                        .name("Some Cab Name")
                        .build(),
                0);


        long jobId = jobService.saveNewJob(JobEntity.builder()
                .start(2)
                .end(8)
                .build());

        RouteEntity route = routeService.getRoute(cabId, 0);

        assertThat(route.getVersion()).isEqualTo(0);
        assertThat(route.getCabId()).isEqualTo(cabId);
        assertThat(route.getJobId()).isEqualTo(jobId);

        assertThat(route.getRouteActions()).hasSize(9);

        assertThat(route.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(0).getDirection()).isEqualTo(RIGHT);
        assertThat(route.getRouteActions().get(0).getMarker()).isEqualTo(1);

        assertThat(route.getRouteActions().get(1).getAction()).isEqualTo(PICKUP);
        assertThat(route.getRouteActions().get(1).getCustomerId()).isEqualTo(jobId);
        assertThat(route.getRouteActions().get(1).getMarker()).isEqualTo(2);

        assertThat(route.getRouteActions().get(2).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(2).getDirection()).isEqualTo(LEFT);
        assertThat(route.getRouteActions().get(2).getMarker()).isEqualTo(4);

        assertThat(route.getRouteActions().get(3).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(3).getDirection()).isEqualTo(RIGHT);
        assertThat(route.getRouteActions().get(3).getMarker()).isEqualTo(7);

        assertThat(route.getRouteActions().get(4).getAction()).isEqualTo(DROPOFF);
        assertThat(route.getRouteActions().get(4).getCustomerId()).isEqualTo(jobId);
        assertThat(route.getRouteActions().get(4).getMarker()).isEqualTo(8);

        assertThat(route.getRouteActions().get(5).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(5).getDirection()).isEqualTo(LEFT);
        assertThat(route.getRouteActions().get(5).getMarker()).isEqualTo(10);

        assertThat(route.getRouteActions().get(6).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(6).getDirection()).isEqualTo(LEFT);
        assertThat(route.getRouteActions().get(6).getMarker()).isEqualTo(12);

        assertThat(route.getRouteActions().get(7).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(7).getDirection()).isEqualTo(RIGHT);
        assertThat(route.getRouteActions().get(7).getMarker()).isEqualTo(14);

        assertThat(route.getRouteActions().get(8).getAction()).isEqualTo(WAIT);
        assertThat(route.getRouteActions().get(8).getMarker()).isEqualTo(0);

        RouteEntity updatedRoute = routeService.getRoute(cabId, 0);

        assertThat(updatedRoute.getVersion()).isEqualTo(0);
        assertThat(updatedRoute.getCabId()).isNull();
        assertThat(updatedRoute.getJobId()).isNull();

        assertThat(updatedRoute.getRouteActions()).isNull();
    }

    @Test
    public void shouldHandleNoPathAvailable() {
        long cabId = registrationService.registerCab(CabEntity.builder()
                        .name("Some Cab Name")
                        .build(),
                1);


        long jobId = jobService.saveNewJob(JobEntity.builder()
                .start(2)
                .end(8)
                .build());

        long blockedCabId = registrationService.registerCab(CabEntity.builder()
                        .name("Some Blocked Cab Name")
                        .build(),
                7);

        blockedService.setBlocked(blockedCabId, true);

        RouteEntity route = routeService.getRoute(cabId, 0);

        assertThat(route.getVersion()).isEqualTo(0);
        assertThat(route.getCabId()).isEqualTo(cabId);
        assertThat(route.getJobId()).isEqualTo(jobId);

        assertThat(route.getRouteActions()).hasSize(4);

        assertThat(route.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(0).getDirection()).isEqualTo(RIGHT);
        assertThat(route.getRouteActions().get(0).getMarker()).isEqualTo(1);

        assertThat(route.getRouteActions().get(1).getAction()).isEqualTo(PICKUP);
        assertThat(route.getRouteActions().get(1).getCustomerId()).isEqualTo(jobId);
        assertThat(route.getRouteActions().get(1).getMarker()).isEqualTo(2);

        assertThat(route.getRouteActions().get(2).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(2).getDirection()).isEqualTo(LEFT);
        assertThat(route.getRouteActions().get(2).getMarker()).isEqualTo(4);

        assertThat(route.getRouteActions().get(3).getAction()).isEqualTo(WAIT);
        assertThat(route.getRouteActions().get(3).getMarker()).isEqualTo(6);

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(2)
                .build());

        pickupService.pickup(cabId, jobId);
        pickupService.acceptPickup(jobId);

        blockedService.setBlocked(blockedCabId, false);

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(6)
                .build());

        RouteEntity updatedRoute = routeService.getRoute(cabId, route.getVersion());

        assertThat(updatedRoute.getVersion()).isEqualTo(1);
        assertThat(updatedRoute.getCabId()).isEqualTo(cabId);
        assertThat(updatedRoute.getJobId()).isEqualTo(jobId);

        assertThat(updatedRoute.getRouteActions()).hasSize(6);

        assertThat(updatedRoute.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(0).getDirection()).isEqualTo(RIGHT);
        assertThat(updatedRoute.getRouteActions().get(0).getMarker()).isEqualTo(7);

        assertThat(updatedRoute.getRouteActions().get(1).getAction()).isEqualTo(DROPOFF);
        assertThat(updatedRoute.getRouteActions().get(1).getCustomerId()).isEqualTo(jobId);
        assertThat(updatedRoute.getRouteActions().get(1).getMarker()).isEqualTo(8);

        assertThat(updatedRoute.getRouteActions().get(2).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(2).getDirection()).isEqualTo(LEFT);
        assertThat(updatedRoute.getRouteActions().get(2).getMarker()).isEqualTo(10);

        assertThat(updatedRoute.getRouteActions().get(3).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(3).getDirection()).isEqualTo(LEFT);
        assertThat(updatedRoute.getRouteActions().get(3).getMarker()).isEqualTo(12);

        assertThat(updatedRoute.getRouteActions().get(4).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(4).getDirection()).isEqualTo(RIGHT);
        assertThat(updatedRoute.getRouteActions().get(4).getMarker()).isEqualTo(14);

        assertThat(updatedRoute.getRouteActions().get(5).getAction()).isEqualTo(WAIT);
        assertThat(updatedRoute.getRouteActions().get(5).getMarker()).isEqualTo(0);

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(8)
                .build());

        dropoffService.dropoff(cabId, jobId);
        dropoffService.acceptDropoff(jobId);

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(0)
                .build());

        RouteEntity updatedRoute2 = routeService.getRoute(cabId, updatedRoute.getVersion());

        assertThat(updatedRoute2.getVersion()).isEqualTo(1);
        assertThat(updatedRoute2.getCabId()).isNull();
        assertThat(updatedRoute2.getJobId()).isNull();
        assertThat(updatedRoute2.getRouteActions()).isNull();
    }

    // TODO (I) job deleted after dropping off (1. happens?, 2. handles correctly? -> next job / to depot [test both possibilities])
    // TODO (II) test IllegalStateException, gone after implementing (I)? If not, maybe also test what happens to state and in controller upon IllegalStateException
    // TODO Route changed immediately upon new job
    // TODO version tests (different constellations)
    // TODO All getRoute paths
    // TODO test multiple jobs at once, so firstAvailable..() etc. are made sure to work as expected, setInProgress() is used where applicable etc.
    // TODO Test if route updates when job is deleted prematurely (before customer is IN_CAB) -> deletion only then possible, and happens automatically upon dropoff
}
