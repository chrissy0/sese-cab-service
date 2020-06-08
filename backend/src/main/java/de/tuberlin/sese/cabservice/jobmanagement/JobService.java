package de.tuberlin.sese.cabservice.jobmanagement;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

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
        return repo.save(entity).getId();
    }

    public void deleteJob(Long id) {
        repo.deleteById(id);
    }
}
