package de.tuberlin.sese.cabservice.persistence.reset;

import de.tuberlin.sese.cabservice.persistence.cab.blocked.CabBlockedRepo;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationRepo;
import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.persistence.customerinteraction.dropoff.DropoffRepo;
import de.tuberlin.sese.cabservice.persistence.customerinteraction.pickup.PickupRepo;
import de.tuberlin.sese.cabservice.persistence.debug.DebugRepo;
import de.tuberlin.sese.cabservice.persistence.job.JobRepo;
import de.tuberlin.sese.cabservice.persistence.route.RouteRepo;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import static de.tuberlin.sese.cabservice.CabserviceApplication.restart;

@Service
@RequiredArgsConstructor
public class ResetService {

    private final CabBlockedRepo cabBlockedRepo;
    private final CabLocationRepo cabLocationRepo;
    private final CabRepo cabRepo;
    private final DropoffRepo dropoffRepo;
    private final PickupRepo pickupRepo;
    private final DebugRepo debugRepo;
    private final JobRepo jobRepo;
    private final RouteRepo routeRepo;

    public void resetAllRepos() {
        cabBlockedRepo.deleteAll();
        cabLocationRepo.deleteAll();
        cabRepo.deleteAll();
        dropoffRepo.deleteAll();
        pickupRepo.deleteAll();
        debugRepo.deleteAll();
        jobRepo.deleteAll();
        routeRepo.deleteAll();
    }

    public void resetSpringBootApp() {
        restart();
    }
}
