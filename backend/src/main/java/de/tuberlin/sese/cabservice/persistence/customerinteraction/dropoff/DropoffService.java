package de.tuberlin.sese.cabservice.persistence.customerinteraction.dropoff;

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
import static de.tuberlin.sese.cabservice.persistence.job.JobEntity.CustomerState.AT_DESTINATION;
import static de.tuberlin.sese.cabservice.persistence.job.JobEntity.CustomerState.IN_CAB;

@Service
@RequiredArgsConstructor
public class DropoffService {

    private final DropoffRepo repo;

    private final CabRepo cabRepo;

    private final CabLocationService locationService;

    private final JobService jobService;

    public void dropoff(Long cabId, Long customerId) {
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
        Integer cabSection = cabLocationOptional.get().getSection();

        if (!cabSection.equals(job.getEnd())) {
            throw new CabCustomerPositionConflictException();
        }

        if (!IN_CAB.equals(job.getCustomerState())) {
            throw new CabCustomerPositionConflictException("Already dropped off customer or never picked up customer");
        }

        repo.save(DropoffRequestEntity.builder()
                .customerId(customerId)
                .cabId(cabId)
                .build());
    }

    public List<DropoffRequestEntity> getDropoffRequests() {
        return Lists.newArrayList(repo.findAll());
    }

    public DropoffCompleteModel dropoffsComplete(Long cabId) {
        if (cabId == null) {
            throw new IllegalArgumentException("Cab ID is null");
        }

        if (!cabRepo.findById(cabId).isPresent()) {
            throw new UnknownCabIdException();
        }

        boolean complete = stream(repo.findAll()).noneMatch(pickup -> cabId.equals(pickup.getCabId()));

        return DropoffCompleteModel.builder()
                .complete(complete)
                .build();
    }

    public void acceptDropoff(Long customerId) {
        if (customerId == null) {
            throw new IllegalArgumentException("Customer ID is null");
        }

        Optional<JobEntity> jobOptional = jobService.getJob(customerId);
        if (!jobOptional.isPresent()) {
            throw new UnknownJobIdException();
        }

        JobEntity job = jobOptional.get();
        job.setCustomerState(AT_DESTINATION);
        jobService.updateJob(job);

        repo.deleteById(customerId);
    }
}
