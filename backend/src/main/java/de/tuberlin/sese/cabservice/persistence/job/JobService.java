package de.tuberlin.sese.cabservice.persistence.job;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static de.tuberlin.sese.cabservice.persistence.job.JobEntity.CustomerState.WAITING;

@Service
@RequiredArgsConstructor
public class JobService {

    private final JobRepo repo;

    public List<JobEntity> getAllJobs() {
        List<JobEntity> entities = new LinkedList<>();
        repo.findAll().forEach(entities::add);
        return entities;
    }

    public Optional<JobEntity> getJob(Long id) {
        return repo.findById(id);
    }

    public List<JobEntity> getAllWaitingJobs() {
        List<JobEntity> entities = new LinkedList<>();
        repo.findAll().forEach(job -> {
            if (!job.isInProgress()) {
                entities.add(job);
            }
        });
        return entities;
    }

    public long saveNewJob(JobEntity entity) {
        entity.setTimestamp(LocalDateTime.now());
        entity.setInProgress(false);
        entity.setCustomerState(WAITING);
        Long id = repo.save(entity).getId();
        entity.setId(id);
        entity.setCustomerId(id);
        repo.save(entity);
        return id;
    }

    public void updateJob(JobEntity entity) {
        repo.save(entity);
    }

    public void deleteJob(Long id) {
        repo.deleteById(id);
    }
}
