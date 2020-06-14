package de.tuberlin.sese.cabservice.persistence.job;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.List;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
public class JobServiceTest {

    @Autowired
    private JobService service;

    @MockBean
    private JobRepo repo;

    @Captor
    private ArgumentCaptor<JobEntity> entityCaptor;

    @Test
    public void shouldGetAllJobs() {
        JobEntity entity1 = JobEntity.builder()
                .start(10)
                .end(11)
                .build();

        JobEntity entity2 = JobEntity.builder()
                .start(12)
                .end(13)
                .build();

        when(repo.findAll()).thenReturn(asList(entity1, entity2));

        List<JobEntity> jobs = service.getAllJobs();

        verify(repo).findAll();
        verifyNoMoreInteractions(repo);

        assertThat(jobs)
                .usingRecursiveFieldByFieldElementComparator()
                .containsExactly(entity1, entity2);
    }

    @Test
    public void shouldSaveJob() {
        JobEntity entity = JobEntity.builder()
                .start(10)
                .end(11)
                .build();

        when(repo.save(entity)).thenReturn(JobEntity.builder()
                .id(1L)
                .build());

        long jobId = service.saveJob(entity);

        assertThat(jobId).isEqualTo(1);

        verify(repo, times(2)).save(entityCaptor.capture());

        List<JobEntity> capturedEntities = entityCaptor.getAllValues();
        assertThat(capturedEntities.get(1).getId()).isEqualTo(1L);
        assertThat(capturedEntities.get(1).getCustomerId()).isEqualTo(1L);
        assertThat(capturedEntities.get(1).getStart()).isEqualTo(10);
        assertThat(capturedEntities.get(1).getEnd()).isEqualTo(11);

        verifyNoMoreInteractions(repo);
    }

    @Test
    public void shouldDeleteJob() {
        service.deleteJob(1L);

        verify(repo).deleteById(1L);
        verifyNoMoreInteractions(repo);
    }
}
