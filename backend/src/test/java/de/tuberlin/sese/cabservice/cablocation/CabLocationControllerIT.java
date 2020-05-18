package de.tuberlin.sese.cabservice.cablocation;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;

import static org.assertj.core.api.Assertions.assertThat;
import static java.util.Arrays.asList;
import static org.mockito.Mockito.*;
import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = CabLocationController.class)
public class CabLocationControllerIT {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private CabLocationService service;

    @Captor
    private ArgumentCaptor<CabLocationEntity> entityCaptor;

    @Test
    public void shouldReturnCabLocations() throws Exception {
        when(service.getAllCabLocations()).thenReturn(asList(
                CabLocationEntity.builder()
                        .cabId(0L)
                        .cabName("some-cab-name")
                        .section(11)
                        .build(),
                CabLocationEntity.builder()
                        .cabId(1L)
                        .cabName("some-other-cab-name")
                        .section(8)
                        .build()));

        mockMvc.perform(get("/api/cab-locations")
                .contentType(APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$[0].cabId").value(0L))
                .andExpect(jsonPath("$[0].cabName").value("some-cab-name"))
                .andExpect(jsonPath("$[0].section").value(11))
                .andExpect(jsonPath("$[1].cabId").value(1L))
                .andExpect(jsonPath("$[1].cabName").value("some-other-cab-name"))
                .andExpect(jsonPath("$[1].section").value(8));

        verify(service).getAllCabLocations();
        verify(service, never()).saveCabLocation(any());
    }

    @Test
    public void shouldSaveCabLocation() throws Exception {
        mockMvc.perform(post("/api/cab-location")
                .contentType(APPLICATION_JSON)
                .content("{\"cabId\": 3, \"cabName\": \"cab-name\", \"section\": 7}"))
                .andExpect(status().isOk());

        verify(service).saveCabLocation(entityCaptor.capture());
        verify(service, never()).getAllCabLocations();

        assertThat(entityCaptor.getValue().getCabId()).isEqualTo(3);
        assertThat(entityCaptor.getValue().getCabName()).isEqualTo("cab-name");
        assertThat(entityCaptor.getValue().getSection()).isEqualTo(7);
    }

    @Test
    public void shouldNotSaveMalformedCabLocation() throws Exception {
        mockMvc.perform(post("/api/cab-location")
                .contentType(APPLICATION_JSON)
                .content("{\"cabId\": \"malformed-cab-id\", \"cabName\": \"cab-name\", \"section\": 7}"))
                .andExpect(status().is4xxClientError());

        verify(service, never()).getAllCabLocations();
        verify(service, never()).saveCabLocation(any());
    }
}

