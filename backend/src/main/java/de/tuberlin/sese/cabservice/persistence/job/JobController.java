package de.tuberlin.sese.cabservice.persistence.job;

import de.tuberlin.sese.cabservice.persistence.route.RouteService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class JobController {

    private final JobService service;

    private final RouteService routeService;

    @GetMapping("/bookr/jobs")
    public List<JobEntity> getJobs() {
        return service.getAllJobs();
    }

    @PostMapping("/bookr/job")
    public void addJob(@RequestBody JobEntity entity) {
        service.saveNewJob(entity);
    }

    @DeleteMapping("/bookr/job")
    public void deleteJob(@RequestParam Long id) {
        routeService.removeJobFromRoutes(id);
        service.deleteJob(id);
    }

}
