package de.tuberlin.sese.cabservice.job;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.annotation.DirtiesContext.ClassMode.AFTER_EACH_TEST_METHOD;

@RunWith(SpringRunner.class)
@SpringBootTest
@DirtiesContext(classMode = AFTER_EACH_TEST_METHOD)
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
        assertThat(savedEntity.getId()).isEqualTo(1);
        assertThat(savedEntity.getStart()).isEqualTo(10);
        assertThat(savedEntity.getEnd()).isEqualTo(11);

        assertThat(savedEntity2).isNotNull();
        assertThat(savedEntity2.getId()).isEqualTo(2);
        assertThat(savedEntity2.getStart()).isEqualTo(12);
        assertThat(savedEntity2.getEnd()).isEqualTo(13);
    }

    @Test
    public void shouldSaveAndLoadJob() {
        repo.save(JobEntity.builder()
                .start(10)
                .end(11)
                .build());

        Optional<JobEntity> loadedEntityOptional = repo.findById(1L);

        assertThat(loadedEntityOptional).isPresent();
        JobEntity loadedEntity = loadedEntityOptional.get();
        assertThat(loadedEntity.getId()).isEqualTo(1L);
        assertThat(loadedEntity.getStart()).isEqualTo(10);
        assertThat(loadedEntity.getEnd()).isEqualTo(11);
    }

    @Test
    public void shouldDeleteJob() {
        repo.save(JobEntity.builder()
                .start(10)
                .end(11)
                .build());

        assertThat(repo.findById(1L)).isPresent();

        repo.deleteById(1L);

        assertThat(repo.findById(1L)).isNotPresent();
    }
}
