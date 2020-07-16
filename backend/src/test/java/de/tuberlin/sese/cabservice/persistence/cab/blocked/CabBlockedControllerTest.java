package de.tuberlin.sese.cabservice.persistence.cab.blocked;

import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = CabBlockedController.class)
public class CabBlockedControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private CabBlockedService service;

    @Test
    public void shouldCallServiceWithCorrectParameters() throws Exception {
        mockMvc.perform(post("/api/ec/blocked")
                .param("cabId", "1")
                .param("blocked", "true"))
                .andExpect(status().isOk());

        verify(service).setBlocked(1L, true);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictOnUnknownCabIdException() throws Exception {
        doThrow(new UnknownCabIdException()).when(service).setBlocked(1L, true);

        mockMvc.perform(post("/api/ec/blocked")
                .param("cabId", "1")
                .param("blocked", "true"))
                .andExpect(status().isConflict());

        verify(service).setBlocked(1L, true);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn400BadRequestOnIllegalArgumentException() throws Exception {
        doThrow(new IllegalArgumentException()).when(service).setBlocked(12L, true);

        mockMvc.perform(post("/api/ec/blocked")
                .param("cabId", "12")
                .param("blocked", "true"))
                .andExpect(status().isBadRequest());

        verify(service).setBlocked(12L, true);
        verifyNoMoreInteractions(service);
    }
}
