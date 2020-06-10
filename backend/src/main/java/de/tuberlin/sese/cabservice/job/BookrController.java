package de.tuberlin.sese.cabservice.job;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/bookr")
@RequiredArgsConstructor
public class BookrController {

    private final JobService service;

    @GetMapping("/jobs")
    public List<JobEntity> getJobs() {
        return service.getAllJobs();
    }

    @PostMapping("/job")
    public void addJob(@RequestBody JobEntity entity) {
        service.saveJob(entity);
    }

    @DeleteMapping("/job")
    public void deleteJob(@RequestParam Long id) {
        service.deleteJob(id);
    }

}
