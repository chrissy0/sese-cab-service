package de.tuberlin.sese.cabservice.persistence.job;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.LinkedList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class JobService {

    private final JobRepo repo;

    public List<JobEntity> getAllJobs() {
        List<JobEntity> entities = new LinkedList<>();
        repo.findAll().forEach(entities::add);
        return entities;
    }

    public long saveJob(JobEntity entity) {
        entity.setTimestamp(LocalDateTime.now());
        Long id = repo.save(entity).getId();
        entity.setId(id);
        entity.setCustomerId(id);
        repo.save(entity);
        return id;
    }

    public void deleteJob(Long id) {
        repo.deleteById(id);
    }
}
