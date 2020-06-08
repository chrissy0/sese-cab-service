package de.tuberlin.sese.cabservice.jobmanagement;

import de.tuberlin.sese.cabservice.cablocation.CabLocationEntity;
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
                .jobId(0L)
                .build());

        long jobId = service.saveJob(entity);

        verify(repo).save(entityCaptor.capture());
        verifyNoMoreInteractions(repo);

        assertThat(entityCaptor.getValue().getJobId()).isNull();
        assertThat(entityCaptor.getValue().getStart()).isEqualTo(10);
        assertThat(entityCaptor.getValue().getEnd()).isEqualTo(11);
        assertThat(jobId).isEqualTo(0);
    }
}
