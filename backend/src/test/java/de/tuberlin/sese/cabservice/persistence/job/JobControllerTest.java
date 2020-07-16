package de.tuberlin.sese.cabservice.persistence.job;

import de.tuberlin.sese.cabservice.persistence.route.RouteService;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;

import java.time.LocalDateTime;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;
import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = JobController.class)
public class JobControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private JobService jobService;

    @MockBean
    private RouteService routeService;

    @Captor
    private ArgumentCaptor<JobEntity> entityCaptor;

    @Test
    public void shouldReturnJobs() throws Exception {
        when(jobService.getAllJobs()).thenReturn(asList(
                JobEntity.builder()
                        .id(1L)
                        .customerId(1L)
                        .start(10)
                        .end(11)
                        .timestamp(LocalDateTime.of(2020, 6, 14, 17, 10))
                        .build(),
                JobEntity.builder()
                        .id(2L)
                        .customerId(2L)
                        .start(12)
                        .end(13)
                        .timestamp(LocalDateTime.of(2020, 6, 14, 17, 11))
                        .build()));

        mockMvc.perform(get("/api/bookr/jobs"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$[0].id").value(1L))
                .andExpect(jsonPath("$[0].customerId").value(1L))
                .andExpect(jsonPath("$[0].start").value(10))
                .andExpect(jsonPath("$[0].end").value(11))
                .andExpect(jsonPath("$[0].timestamp").exists())
                .andExpect(jsonPath("$[1].id").value(2L))
                .andExpect(jsonPath("$[1].customerId").value(2L))
                .andExpect(jsonPath("$[1].start").value(12))
                .andExpect(jsonPath("$[1].end").value(13))
                .andExpect(jsonPath("$[1].timestamp").exists());

        verify(jobService).getAllJobs();
        verifyNoMoreInteractions(jobService);
    }

    @Test
    public void shouldSaveJob() throws Exception {
        mockMvc.perform(post("/api/bookr/job")
                .contentType(APPLICATION_JSON)
                .content("{\"start\": 11,\"end\": 12}"))
                .andExpect(status().isOk());

        verify(jobService).saveNewJob(entityCaptor.capture());
        verifyNoMoreInteractions(jobService);

        assertThat(entityCaptor.getValue().getStart()).isEqualTo(11);
        assertThat(entityCaptor.getValue().getEnd()).isEqualTo(12);
    }

    @Test
    public void shouldNotSaveMalformedJob() throws Exception {
        mockMvc.perform(post("/bookr/job")
                .contentType(APPLICATION_JSON)
                .content("{\"start\": \"malformed-start-station\",\"end\": 12}"))
                .andExpect(status().is4xxClientError());

        verifyNoMoreInteractions(jobService);
    }

    @Test
    public void shouldNotSaveJobContainingTimestamp() throws Exception {
        System.out.println(LocalDateTime.now());
        mockMvc.perform(post("/bookr/job")
                .contentType(APPLICATION_JSON)
                .content("{\"start\": \"11\",\"end\": 12, \"timestamp\": \"2020-06-14T16:09:50.207\"}"))
                .andExpect(status().is4xxClientError());

        verifyNoMoreInteractions(jobService);
    }

    @Test
    public void shouldDeleteJob() throws Exception {
        mockMvc.perform(delete("/api/bookr/job")
                .param("id", "1"))
                .andExpect(status().isOk());

        verify(routeService).removeJobFromRoutes(1L);
        verify(jobService).deleteJob(1L);

        verifyNoMoreInteractions(routeService);
        verifyNoMoreInteractions(jobService);
    }
}
