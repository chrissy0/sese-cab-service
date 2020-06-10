package de.tuberlin.sese.cabservice.cab.registration;

import de.tuberlin.sese.cabservice.cab.registration.persistence.CabEntity;
import de.tuberlin.sese.cabservice.util.exceptions.NameAlreadyInUseException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownSectionException;
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
import static org.mockito.Mockito.*;
import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = CabRegistrationController.class)
public class CabRegistrationControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private CabRegistrationService service;

    @Captor
    private ArgumentCaptor<CabEntity> entityCaptor;

    @Test
    public void shouldRegisterCab() throws Exception {
        when(service.registerCab(any(CabEntity.class), any(Integer.class))).thenReturn(13L);

        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabName\": \"Some Cab Name\", \"section\": 3}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(13L));

        verify(service).registerCab(entityCaptor.capture(), eq(3));
        verifyNoMoreInteractions(service);

        CabEntity capturedEntity = entityCaptor.getValue();
        assertThat(capturedEntity.getId()).isNull();
        assertThat(capturedEntity.getName()).isEqualTo("Some Cab Name");
    }

    @Test
    public void shouldReturn409ConflictOnNameAlreadyInUseException() throws Exception {
        when(service.registerCab(any(CabEntity.class), any(Integer.class))).thenThrow(new NameAlreadyInUseException());

        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabName\": \"Some Cab Name\", \"section\": 3}"))
                .andExpect(status().isConflict());
    }

    @Test
    public void shouldReturn409ConflictOnUnknownSectionException() throws Exception {
        when(service.registerCab(any(CabEntity.class), any(Integer.class))).thenThrow(new UnknownSectionException());

        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabName\": \"Some Cab Name\", \"section\": 3}"))
                .andExpect(status().isConflict());
    }

    @Test
    public void shouldReturn400BadRequestOnIllegalArgumentException() throws Exception {
        when(service.registerCab(any(CabEntity.class), any(Integer.class))).thenThrow(new IllegalArgumentException());

        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabName\": \"Some Cab Name\", \"section\": 3}"))
                .andExpect(status().isBadRequest());
    }
}
