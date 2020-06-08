package de.tuberlin.sese.cabservice.jobmanagement;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

import static org.assertj.core.api.Assertions.assertThat;

@RunWith(SpringRunner.class)
@SpringBootTest
public class JobRepoTest {

    @Autowired
    private JobRepo repo;

    @Test
    public void shouldAddJobIdToJobEntity() {
        JobEntity savedEntity = repo.save(JobEntity.builder()
                .start(10)
                .end(11)
                .build());

        JobEntity savedEntity2 = repo.save(JobEntity.builder()
                .start(12)
                .end(13)
                .build());

        assertThat(savedEntity).isNotNull();
        assertThat(savedEntity.getJobId()).isEqualTo(1);
        assertThat(savedEntity.getStart()).isEqualTo(10);
        assertThat(savedEntity.getEnd()).isEqualTo(11);

        assertThat(savedEntity2).isNotNull();
        assertThat(savedEntity2.getJobId()).isEqualTo(2);
        assertThat(savedEntity2.getStart()).isEqualTo(12);
        assertThat(savedEntity2.getEnd()).isEqualTo(13);
    }
}
