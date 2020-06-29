package de.tuberlin.sese.cabservice.persistence.customerinteraction.pickup;

import com.google.common.collect.Lists;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationEntity;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationService;
import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.persistence.job.JobEntity;
import de.tuberlin.sese.cabservice.persistence.job.JobService;
import de.tuberlin.sese.cabservice.util.exceptions.CabCustomerPositionConflictException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabLocationException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownJobIdException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import static com.google.common.collect.Streams.stream;
import static de.tuberlin.sese.cabservice.persistence.job.JobEntity.CustomerState.IN_CAB;
import static de.tuberlin.sese.cabservice.persistence.job.JobEntity.CustomerState.WAITING;

@Service
@RequiredArgsConstructor
public class PickupService {

    private final PickupRepo pickupRepo;

    private final CabRepo cabRepo;

    private final CabLocationService locationService;

    private final JobService jobService;

    @SuppressWarnings("DuplicatedCode")
    public void pickup(Long cabId, Long customerId) {
        if (cabId == null || customerId == null) {
            throw new IllegalArgumentException("Cab ID or customer ID is null");
        }

        if (!cabRepo.findById(cabId).isPresent()) {
            throw new UnknownCabIdException();
        }

        Optional<CabLocationEntity> cabLocationOptional = locationService.getCabLocation(cabId);
        if (!cabLocationOptional.isPresent()) {
            throw new UnknownCabLocationException();
        }
        Optional<JobEntity> jobOptional = jobService.getJob(customerId);
        if (!jobOptional.isPresent()) {
            throw new UnknownJobIdException();
        }

        JobEntity job = jobOptional.get();
        Integer customerSection = job.getStart();
        Integer cabSection = cabLocationOptional.get().getSection();

        if (!cabSection.equals(customerSection)) {
            throw new CabCustomerPositionConflictException();
        }

        if (!WAITING.equals(job.getCustomerState())) {
            throw new CabCustomerPositionConflictException("Already picked up customer");
        }

        pickupRepo.save(PickupRequestEntity.builder()
                .customerId(customerId)
                .cabId(cabId)
                .build());
    }

    public List<PickupRequestEntity> getPickupRequests() {
        return Lists.newArrayList(pickupRepo.findAll());
    }

    // TODO test multiple pickups
    public PickupCompleteModel pickupsComplete(Long cabId) {
        if (cabId == null) {
            throw new IllegalArgumentException("Cab ID is null");
        }

        if (!cabRepo.findById(cabId).isPresent()) {
            throw new UnknownCabIdException();
        }

        boolean complete = stream(pickupRepo.findAll())
                .filter(pickup -> {
                    if (!jobService.getJob(pickup.getCustomerId()).isPresent()) {
                        // If job was deleted after pickup was requested, pickup request should be deleted
                        // Cab receives "completed: true" if no other pickups are in progress
                        // Cab should always request new route after pickup so it doesn't attempt dropping
                        // off customer it never picked up
                        pickupRepo.deleteById(pickup.getCustomerId());
                        return false;
                    }
                    return true;
                })
                .noneMatch(pickup -> cabId.equals(pickup.getCabId()));

        return PickupCompleteModel.builder()
                .complete(complete)
                .build();
    }

    @SuppressWarnings("DuplicatedCode")
    public void acceptPickup(Long customerId) {
        if (customerId == null) {
            throw new IllegalArgumentException("Customer ID is null");
        }

        Optional<JobEntity> jobOptional = jobService.getJob(customerId);
        if (!jobOptional.isPresent()) {
            throw new UnknownJobIdException();
        }

        JobEntity job = jobOptional.get();
        job.setCustomerState(IN_CAB);
        jobService.updateJob(job);

        pickupRepo.deleteById(customerId);
    }
}
