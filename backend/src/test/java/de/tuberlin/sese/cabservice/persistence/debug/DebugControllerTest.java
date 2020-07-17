package de.tuberlin.sese.cabservice.persistence.debug;

import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;

import static java.util.Arrays.asList;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = DebugController.class)
public class DebugControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private DebugService debugService;

    @Test
    public void shouldReturnCabSensorStatus() throws Exception {
        when(debugService.getCabSensorStatus(1L)).thenReturn(CabSensorStatus.builder()
                .sensorErrors(true)
                .sensors(asList(
                        SensorStatusEntity.builder()
                                .id(1L)
                                .cabId(1L)
                                .name("inf_rm_br")
                                .disabled(false)
                                .noise(15)
                                .build(),
                        SensorStatusEntity.builder()
                                .id(2L)
                                .cabId(1L)
                                .name("inf_rm_br2")
                                .disabled(true)
                                .noise(0)
                                .build()))
                .build());

        mockMvc.perform(get("/api/ec/sensorStatus")
                .param("cabId", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.sensorErrors").value(true))
                .andExpect(jsonPath("$.sensors[0].name").value("inf_rm_br"))
                .andExpect(jsonPath("$.sensors[0].disabled").value(false))
                .andExpect(jsonPath("$.sensors[0].whoosh").value(15))
                .andExpect(jsonPath("$.sensors[1].name").value("inf_rm_br2"))
                .andExpect(jsonPath("$.sensors[1].disabled").value(true))
                .andExpect(jsonPath("$.sensors[1].whoosh").value(0));

        verify(debugService).getCabSensorStatus(1L);
        verifyNoMoreInteractions(debugService);
    }

    @Test
    public void shouldReturnCabSensorStatusWithoutSensorErrors() throws Exception {
        when(debugService.getCabSensorStatus(1L)).thenReturn(CabSensorStatus.builder()
                .sensorErrors(false)
                .sensors(null)
                .build());

        mockMvc.perform(get("/api/ec/sensorStatus")
                .param("cabId", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.sensorErrors").value(false))
                .andExpect(jsonPath("$.sensors").doesNotExist());

        verify(debugService).getCabSensorStatus(1L);
        verifyNoMoreInteractions(debugService);
    }

    @Test
    public void shouldReturn409ConflictOnUnknownCabIdException() throws Exception {
        doThrow(new UnknownCabIdException()).when(debugService).getCabSensorStatus(1L);

        mockMvc.perform(get("/api/ec/sensorStatus")
                .param("cabId", "1"))
                .andExpect(status().isConflict());

        verify(debugService).getCabSensorStatus(1L);
        verifyNoMoreInteractions(debugService);
    }

    @Test
    public void shouldReturn400BadRequestOnIllegalArgumentException() throws Exception {
        doThrow(new IllegalArgumentException()).when(debugService).getCabSensorStatus(1L);

        mockMvc.perform(get("/api/ec/sensorStatus")
                .param("cabId", "1"))
                .andExpect(status().isBadRequest());

        verify(debugService).getCabSensorStatus(1L);
        verifyNoMoreInteractions(debugService);
    }
}
