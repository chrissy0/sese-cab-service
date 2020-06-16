package de.tuberlin.sese.cabservice.persistence.job;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.annotation.DirtiesContext.ClassMode.BEFORE_EACH_TEST_METHOD;

@RunWith(SpringRunner.class)
@SpringBootTest
@DirtiesContext(classMode = BEFORE_EACH_TEST_METHOD)
public class JobServiceIT {

    @Autowired
    private JobService service;

    @Test
    public void shouldOnlySaveOneEntity() {

        service.saveJob(JobEntity.builder()
                .start(10)
                .end(11)
                .build());

        List<JobEntity> allJobs = service.getAllJobs();
        assertThat(allJobs).hasSize(1);
        JobEntity job = allJobs.get(0);
        assertThat(job.getStart()).isEqualTo(10);
        assertThat(job.getEnd()).isEqualTo(11);
        assertThat(job.getId()).isEqualTo(job.getCustomerId());
        assertThat(job.getTimestamp()).isNotNull();
    }
}
