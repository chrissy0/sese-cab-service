package de.tuberlin.sese.cabservice.jobmanagement;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;
import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = BookrController.class)
public class BookrControllerIT {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private JobService service;

    @Captor
    private ArgumentCaptor<JobEntity> entityCaptor;

    @Test
    public void shouldReturnJobs() throws Exception {
        when(service.getAllJobs()).thenReturn(asList(
                JobEntity.builder()
                        .jobId(1L)
                        .startStation(10)
                        .endStation(11)
                        .build(),
                JobEntity.builder()
                        .jobId(2L)
                        .startStation(12)
                        .endStation(13)
                        .build()));

        mockMvc.perform(get("/bookr/jobs")
                .contentType(APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$[0].jobId").value(1L))
                .andExpect(jsonPath("$[0].startStation").value(10))
                .andExpect(jsonPath("$[0].endStation").value(11))
                .andExpect(jsonPath("$[1].jobId").value(2L))
                .andExpect(jsonPath("$[1].startStation").value(12))
                .andExpect(jsonPath("$[1].endStation").value(13));

        verify(service).getAllJobs();
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldSaveJob() throws Exception {
        mockMvc.perform(post("/bookr/job")
                .contentType(APPLICATION_JSON)
                .content("{\"startStation\": 11,\"endStation\": 12}"))
                .andExpect(status().isOk());

        verify(service).saveJob(entityCaptor.capture());
        verifyNoMoreInteractions(service);

        assertThat(entityCaptor.getValue().getStartStation()).isEqualTo(11);
        assertThat(entityCaptor.getValue().getEndStation()).isEqualTo(12);
    }

    @Test
    public void shouldNotSaveMalformedJob() throws Exception {
        mockMvc.perform(post("/bookr/job")
                .contentType(APPLICATION_JSON)
                .content("{\"startStation\": \"malformed-start-station\",\"endStation\": 12}"))
                .andExpect(status().is4xxClientError());

        verifyNoMoreInteractions(service);
    }
}
