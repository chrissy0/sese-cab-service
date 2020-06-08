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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
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
                        .id(1L)
                        .start(10)
                        .end(11)
                        .build(),
                JobEntity.builder()
                        .id(2L)
                        .start(12)
                        .end(13)
                        .build()));

        mockMvc.perform(get("/bookr/jobs")
                .contentType(APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$[0].id").value(1L))
                .andExpect(jsonPath("$[0].start").value(10))
                .andExpect(jsonPath("$[0].end").value(11))
                .andExpect(jsonPath("$[1].id").value(2L))
                .andExpect(jsonPath("$[1].start").value(12))
                .andExpect(jsonPath("$[1].end").value(13));

        verify(service).getAllJobs();
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldSaveJob() throws Exception {
        mockMvc.perform(post("/bookr/job")
                .contentType(APPLICATION_JSON)
                .content("{\"start\": 11,\"end\": 12}"))
                .andExpect(status().isOk());

        verify(service).saveJob(entityCaptor.capture());
        verifyNoMoreInteractions(service);

        assertThat(entityCaptor.getValue().getStart()).isEqualTo(11);
        assertThat(entityCaptor.getValue().getEnd()).isEqualTo(12);
    }

    @Test
    public void shouldNotSaveMalformedJob() throws Exception {
        mockMvc.perform(post("/bookr/job")
                .contentType(APPLICATION_JSON)
                .content("{\"start\": \"malformed-start-station\",\"end\": 12}"))
                .andExpect(status().is4xxClientError());

        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldDeleteJob() throws Exception {
        mockMvc.perform(delete("/bookr/job")
                .param("id", "1"))
                .andExpect(status().isOk());

        verify(service).deleteJob(1L);
        verifyNoMoreInteractions(service);
    }
}
