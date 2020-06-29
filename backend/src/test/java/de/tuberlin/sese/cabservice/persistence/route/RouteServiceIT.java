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
    public void shouldReturnRouteToDepotForNoJobWaiting() {
        long cabId = registrationService.registerCab(CabEntity.builder()
                        .name("Some Cab Name")
                        .build(),
                9);

        RouteEntity route = routeService.getRoute(cabId, 0);

        assertThat(route.getVersion()).isEqualTo(0);
        assertThat(route.getCabId()).isEqualTo(cabId);
        assertThat(route.getJobId()).isNull();

        assertThat(route.getRouteActions().get(0).getMarker()).isEqualTo(10);
        assertThat(route.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(0).getDirection()).isEqualTo(LEFT);

        assertThat(route.getRouteActions().get(1).getMarker()).isEqualTo(12);
        assertThat(route.getRouteActions().get(1).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(1).getDirection()).isEqualTo(LEFT);

        assertThat(route.getRouteActions().get(2).getMarker()).isEqualTo(14);
        assertThat(route.getRouteActions().get(2).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(2).getDirection()).isEqualTo(RIGHT);

        assertThat(route.getRouteActions().get(3).getMarker()).isEqualTo(0);
        assertThat(route.getRouteActions().get(3).getAction()).isEqualTo(WAIT);

        assertThat(route.getRouteActions()).hasSize(4);
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

    @SuppressWarnings("OptionalGetWithoutIsPresent")
    @Test
    public void shouldStartSecondJobImmediatelyAfterFinishingFirstAndSetInProgressCorrectly() {
        long cabId = registrationService.registerCab(CabEntity.builder()
                .name("Some Cab Name")
                .build(), 0);

        RouteEntity preJobRoute = routeService.getRoute(cabId, 0);

        assertThat(preJobRoute.getVersion()).isEqualTo(0);
        assertThat(preJobRoute.getCabId()).isEqualTo(cabId);
        assertThat(preJobRoute.getJobId()).isNull();

        assertThat(preJobRoute.getRouteActions().get(0).getMarker()).isEqualTo(0);
        assertThat(preJobRoute.getRouteActions().get(0).getAction()).isEqualTo(WAIT);

        long jobId1 = jobService.saveNewJob(JobEntity.builder()
                .start(2)
                .end(8)
                .build());


        long jobId2 = jobService.saveNewJob(JobEntity.builder()
                .start(2)
                .end(11)
                .build());

        RouteEntity route = routeService.getRoute(cabId, preJobRoute.getVersion() + 1);

        assertThat(jobService.getJob(jobId1).get().isInProgress()).isTrue();
        assertThat(jobService.getJob(jobId2).get().isInProgress()).isFalse();

        assertThat(route.getVersion()).isEqualTo(preJobRoute.getVersion() + 1);
        assertThat(route.getCabId()).isEqualTo(cabId);
        assertThat(route.getJobId()).isEqualTo(jobId1);

        assertThat(route.getRouteActions().get(0).getMarker()).isEqualTo(1);
        assertThat(route.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(0).getDirection()).isEqualTo(RIGHT);

        assertThat(route.getRouteActions().get(1).getMarker()).isEqualTo(2);
        assertThat(route.getRouteActions().get(1).getAction()).isEqualTo(PICKUP);
        assertThat(route.getRouteActions().get(1).getCustomerId()).isEqualTo(jobId1);

        assertThat(route.getRouteActions().get(2).getMarker()).isEqualTo(4);
        assertThat(route.getRouteActions().get(2).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(2).getDirection()).isEqualTo(LEFT);

        assertThat(route.getRouteActions().get(3).getMarker()).isEqualTo(7);
        assertThat(route.getRouteActions().get(3).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(3).getDirection()).isEqualTo(RIGHT);

        assertThat(route.getRouteActions().get(4).getMarker()).isEqualTo(8);
        assertThat(route.getRouteActions().get(4).getAction()).isEqualTo(DROPOFF);
        assertThat(route.getRouteActions().get(4).getCustomerId()).isEqualTo(jobId1);

        assertThat(route.getRouteActions().get(5).getMarker()).isEqualTo(10);
        assertThat(route.getRouteActions().get(5).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(5).getDirection()).isEqualTo(LEFT);

        assertThat(route.getRouteActions().get(6).getMarker()).isEqualTo(12);
        assertThat(route.getRouteActions().get(6).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(6).getDirection()).isEqualTo(LEFT);

        assertThat(route.getRouteActions().get(7).getMarker()).isEqualTo(14);
        assertThat(route.getRouteActions().get(7).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(7).getDirection()).isEqualTo(RIGHT);

        assertThat(route.getRouteActions().get(8).getMarker()).isEqualTo(0);
        assertThat(route.getRouteActions().get(8).getAction()).isEqualTo(WAIT);

        assertThat(route.getRouteActions()).hasSize(9);

        assertThat(jobService.getJob(jobId1)).isPresent();

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(2)
                .build());

        pickupService.pickup(cabId, jobId1);
        pickupService.acceptPickup(jobId1);

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(8)
                .build());

        dropoffService.dropoff(cabId, jobId1);
        dropoffService.acceptDropoff(jobId1);

        assertThat(jobService.getJob(jobId1)).isNotPresent();

        RouteEntity updatedRoute = routeService.getRoute(cabId, route.getVersion());

        assertThat(jobService.getJob(jobId2).get().isInProgress()).isTrue();

        assertThat(updatedRoute.getVersion()).isEqualTo(route.getVersion() + 1);
        assertThat(updatedRoute.getCabId()).isEqualTo(cabId);
        assertThat(updatedRoute.getJobId()).isEqualTo(jobId2);

        assertThat(updatedRoute.getRouteActions().get(0).getMarker()).isEqualTo(10);
        assertThat(updatedRoute.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(0).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(1).getMarker()).isEqualTo(12);
        assertThat(updatedRoute.getRouteActions().get(1).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(1).getDirection()).isEqualTo(RIGHT);

        assertThat(updatedRoute.getRouteActions().get(2).getMarker()).isEqualTo(1);
        assertThat(updatedRoute.getRouteActions().get(2).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(2).getDirection()).isEqualTo(RIGHT);

        assertThat(updatedRoute.getRouteActions().get(3).getMarker()).isEqualTo(2);
        assertThat(updatedRoute.getRouteActions().get(3).getAction()).isEqualTo(PICKUP);
        assertThat(updatedRoute.getRouteActions().get(3).getCustomerId()).isEqualTo(jobId2);

        assertThat(updatedRoute.getRouteActions().get(4).getMarker()).isEqualTo(4);
        assertThat(updatedRoute.getRouteActions().get(4).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(4).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(5).getMarker()).isEqualTo(7);
        assertThat(updatedRoute.getRouteActions().get(5).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(5).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(6).getMarker()).isEqualTo(10);
        assertThat(updatedRoute.getRouteActions().get(6).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(6).getDirection()).isEqualTo(RIGHT);

        assertThat(updatedRoute.getRouteActions().get(7).getMarker()).isEqualTo(11);
        assertThat(updatedRoute.getRouteActions().get(7).getAction()).isEqualTo(DROPOFF);
        assertThat(updatedRoute.getRouteActions().get(7).getCustomerId()).isEqualTo(jobId2);

        assertThat(updatedRoute.getRouteActions().get(8).getMarker()).isEqualTo(1);
        assertThat(updatedRoute.getRouteActions().get(8).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(8).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(9).getMarker()).isEqualTo(4);
        assertThat(updatedRoute.getRouteActions().get(9).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(9).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(10).getMarker()).isEqualTo(7);
        assertThat(updatedRoute.getRouteActions().get(10).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(10).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(11).getMarker()).isEqualTo(10);
        assertThat(updatedRoute.getRouteActions().get(11).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(11).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(12).getMarker()).isEqualTo(12);
        assertThat(updatedRoute.getRouteActions().get(12).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(12).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(13).getMarker()).isEqualTo(14);
        assertThat(updatedRoute.getRouteActions().get(13).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(13).getDirection()).isEqualTo(RIGHT);

        assertThat(updatedRoute.getRouteActions().get(14).getMarker()).isEqualTo(0);
        assertThat(updatedRoute.getRouteActions().get(14).getAction()).isEqualTo(WAIT);

        assertThat(updatedRoute.getRouteActions()).hasSize(15);

        assertThat(jobService.getJob(jobId2)).isPresent();

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(2)
                .build());

        pickupService.pickup(cabId, jobId2);
        pickupService.acceptPickup(jobId2);

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(11)
                .build());

        dropoffService.dropoff(cabId, jobId2);
        dropoffService.acceptDropoff(jobId2);

        assertThat(jobService.getJob(jobId2)).isNotPresent();

        RouteEntity updatedRoute2 = routeService.getRoute(cabId, updatedRoute.getVersion());

        assertThat(updatedRoute2.getVersion()).isEqualTo(updatedRoute.getVersion());
        assertThat(updatedRoute2.getCabId()).isNull();
        assertThat(updatedRoute2.getJobId()).isNull();
        assertThat(updatedRoute2.getRouteActions()).isNull();
    }

    @Test
    public void shouldNotGiveJobToCabInDepot() {
        long cabId = registrationService.registerCab(CabEntity.builder()
                        .name("Some Cab Name")
                        .build(),
                14);

        long jobId = jobService.saveNewJob(JobEntity.builder()
                .start(2)
                .end(8)
                .build());

        RouteEntity route = routeService.getRoute(cabId, 0);

        assertThat(route.getVersion()).isEqualTo(0);
        assertThat(route.getCabId()).isEqualTo(cabId);
        assertThat(route.getJobId()).isNull();

        assertThat(route.getRouteActions().get(0).getMarker()).isEqualTo(14);
        assertThat(route.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(0).getDirection()).isEqualTo(RIGHT);

        assertThat(route.getRouteActions().get(1).getMarker()).isEqualTo(0);
        assertThat(route.getRouteActions().get(1).getAction()).isEqualTo(WAIT);

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(0)
                .build());

        RouteEntity updatedRoute = routeService.getRoute(cabId, 0);

        assertThat(updatedRoute.getVersion()).isEqualTo(route.getVersion() + 1);
        assertThat(updatedRoute.getCabId()).isEqualTo(cabId);
        assertThat(updatedRoute.getJobId()).isEqualTo(jobId);

        assertThat(updatedRoute.getRouteActions().get(0).getMarker()).isEqualTo(1);
        assertThat(updatedRoute.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(0).getDirection()).isEqualTo(RIGHT);
    }

    // This test implicitly verifies, that the routing algorithm can handle an existing
    // route which has a job id of a non-existing job associated with it
    @Test
    public void shouldRerouteIfJobIsDeletedPrematurelyEvenIfJobIdWasNotRemovedFromRoute() {
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

        assertThat(route.getRouteActions().get(0).getMarker()).isEqualTo(1);
        assertThat(route.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(0).getDirection()).isEqualTo(RIGHT);

        assertThat(route.getRouteActions().get(1).getMarker()).isEqualTo(2);
        assertThat(route.getRouteActions().get(1).getAction()).isEqualTo(PICKUP);
        assertThat(route.getRouteActions().get(1).getCustomerId()).isEqualTo(jobId);

        assertThat(route.getRouteActions().get(2).getMarker()).isEqualTo(4);
        assertThat(route.getRouteActions().get(2).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(2).getDirection()).isEqualTo(LEFT);

        assertThat(route.getRouteActions().get(3).getMarker()).isEqualTo(7);
        assertThat(route.getRouteActions().get(3).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(3).getDirection()).isEqualTo(RIGHT);

        assertThat(route.getRouteActions().get(4).getMarker()).isEqualTo(8);
        assertThat(route.getRouteActions().get(4).getAction()).isEqualTo(DROPOFF);
        assertThat(route.getRouteActions().get(4).getCustomerId()).isEqualTo(jobId);

        assertThat(route.getRouteActions().get(5).getMarker()).isEqualTo(10);
        assertThat(route.getRouteActions().get(5).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(5).getDirection()).isEqualTo(LEFT);

        assertThat(route.getRouteActions().get(6).getMarker()).isEqualTo(12);
        assertThat(route.getRouteActions().get(6).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(6).getDirection()).isEqualTo(LEFT);

        assertThat(route.getRouteActions().get(7).getMarker()).isEqualTo(14);
        assertThat(route.getRouteActions().get(7).getAction()).isEqualTo(TURN);
        assertThat(route.getRouteActions().get(7).getDirection()).isEqualTo(RIGHT);

        assertThat(route.getRouteActions().get(8).getMarker()).isEqualTo(0);
        assertThat(route.getRouteActions().get(8).getAction()).isEqualTo(WAIT);

        assertThat(route.getRouteActions()).hasSize(9);

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(1)
                .build());

        jobService.deleteJob(jobId);

        RouteEntity updatedRoute = routeService.getRoute(cabId, route.getVersion());

        assertThat(updatedRoute.getVersion()).isEqualTo(route.getVersion() + 1);
        assertThat(updatedRoute.getCabId()).isEqualTo(cabId);
        assertThat(updatedRoute.getJobId()).isNull();

        assertThat(updatedRoute.getRouteActions().get(0).getMarker()).isEqualTo(1);
        assertThat(updatedRoute.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(0).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(1).getMarker()).isEqualTo(4);
        assertThat(updatedRoute.getRouteActions().get(1).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(1).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(2).getMarker()).isEqualTo(7);
        assertThat(updatedRoute.getRouteActions().get(2).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(2).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(3).getMarker()).isEqualTo(10);
        assertThat(updatedRoute.getRouteActions().get(3).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(3).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(4).getMarker()).isEqualTo(12);
        assertThat(updatedRoute.getRouteActions().get(4).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(4).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(5).getMarker()).isEqualTo(14);
        assertThat(updatedRoute.getRouteActions().get(5).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(5).getDirection()).isEqualTo(RIGHT);

        assertThat(updatedRoute.getRouteActions().get(6).getMarker()).isEqualTo(0);
        assertThat(updatedRoute.getRouteActions().get(6).getAction()).isEqualTo(WAIT);

        assertThat(updatedRoute.getRouteActions()).hasSize(7);
    }

    @Test
    public void shouldHandleRouteExistsAndJobIdIsAvailableAndJobExistsAndRouteIsSubrouteOfSavedRoute() {
        long cabId = registrationService.registerCab(CabEntity.builder()
                        .name("Some Cab Name")
                        .build(),
                0);

        long jobId = jobService.saveNewJob(JobEntity.builder()
                .start(5)
                .end(8)
                .build());

        RouteEntity route = routeService.getRoute(cabId, 0);

        assertThat(route.getVersion()).isEqualTo(0);
        assertThat(route.getCabId()).isEqualTo(cabId);
        assertThat(route.getJobId()).isEqualTo(jobId);

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(1)
                .build());

        RouteEntity updatedRoute = routeService.getRoute(cabId, route.getVersion());

        assertThat(updatedRoute.getVersion()).isEqualTo(route.getVersion());
        assertThat(updatedRoute.getCabId()).isNull();
        assertThat(updatedRoute.getJobId()).isNull();
        assertThat(updatedRoute.getRouteActions()).isNull();
    }

    @Test
    public void shouldHandleRouteExistsAndJobIdIsAvailableAndJobExistsAndRouteIsNotSubrouteOfSavedRoute() {
        long cabId = registrationService.registerCab(CabEntity.builder()
                        .name("Some Cab Name")
                        .build(),
                0);

        long jobId = jobService.saveNewJob(JobEntity.builder()
                .start(5)
                .end(8)
                .build());

        RouteEntity route = routeService.getRoute(cabId, 0);

        assertThat(route.getVersion()).isEqualTo(0);
        assertThat(route.getCabId()).isEqualTo(cabId);
        assertThat(route.getJobId()).isEqualTo(jobId);

        locationService.saveCabLocation(CabLocationEntity.builder()
                .cabId(cabId)
                .section(10)
                .build());

        RouteEntity updatedRoute = routeService.getRoute(cabId, route.getVersion());

        assertThat(updatedRoute.getVersion()).isEqualTo(route.getVersion() + 1);
        assertThat(updatedRoute.getCabId()).isEqualTo(cabId);
        assertThat(updatedRoute.getJobId()).isEqualTo(jobId);

        assertThat(updatedRoute.getRouteActions().get(0).getMarker()).isEqualTo(10);
        assertThat(updatedRoute.getRouteActions().get(0).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(0).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(1).getMarker()).isEqualTo(12);
        assertThat(updatedRoute.getRouteActions().get(1).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(1).getDirection()).isEqualTo(RIGHT);

        assertThat(updatedRoute.getRouteActions().get(2).getMarker()).isEqualTo(1);
        assertThat(updatedRoute.getRouteActions().get(2).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(2).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(3).getMarker()).isEqualTo(4);
        assertThat(updatedRoute.getRouteActions().get(3).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(3).getDirection()).isEqualTo(RIGHT);

        assertThat(updatedRoute.getRouteActions().get(4).getMarker()).isEqualTo(5);
        assertThat(updatedRoute.getRouteActions().get(4).getAction()).isEqualTo(PICKUP);
        assertThat(updatedRoute.getRouteActions().get(4).getCustomerId()).isEqualTo(jobId);

        assertThat(updatedRoute.getRouteActions().get(5).getMarker()).isEqualTo(7);
        assertThat(updatedRoute.getRouteActions().get(5).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(5).getDirection()).isEqualTo(RIGHT);

        assertThat(updatedRoute.getRouteActions().get(6).getMarker()).isEqualTo(8);
        assertThat(updatedRoute.getRouteActions().get(6).getAction()).isEqualTo(DROPOFF);
        assertThat(updatedRoute.getRouteActions().get(6).getCustomerId()).isEqualTo(jobId);

        assertThat(updatedRoute.getRouteActions().get(7).getMarker()).isEqualTo(10);
        assertThat(updatedRoute.getRouteActions().get(7).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(7).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(8).getMarker()).isEqualTo(12);
        assertThat(updatedRoute.getRouteActions().get(8).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(8).getDirection()).isEqualTo(LEFT);

        assertThat(updatedRoute.getRouteActions().get(9).getMarker()).isEqualTo(14);
        assertThat(updatedRoute.getRouteActions().get(9).getAction()).isEqualTo(TURN);
        assertThat(updatedRoute.getRouteActions().get(9).getDirection()).isEqualTo(RIGHT);

        assertThat(updatedRoute.getRouteActions().get(10).getMarker()).isEqualTo(0);
        assertThat(updatedRoute.getRouteActions().get(10).getAction()).isEqualTo(WAIT);
    }

    // TODO Test all getRoute paths including version
    // DONE route exists, job id available, job exists, route is subroute of saved route
    // DONE route exists, job id available, job exists, route is not subroute of saved route
    // DONE route exists, job id available, job does not exist
    // DONE route exists, job id unavailable, new job available
    // DONE route exists, job id unavailable, no new job available
    // DONE route does not exist, new job is available and cab in eligible section
    // DONE route does not exist, new job is not available
}
