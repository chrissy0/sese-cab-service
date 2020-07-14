package de.tuberlin.sese.cabservice.persistence.customerinteraction.pickup;

import de.tuberlin.sese.cabservice.util.exceptions.CabCustomerPositionConflictException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabLocationException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownJobIdException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = PickupController.class)
public class PickupControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private PickupService service;

    @Test
    public void shouldRequestPickup() throws Exception {

        mockMvc.perform(post("/api/ec/requestPickup")
                .param("cabId", "1")
                .param("customerId", "1"))
                .andExpect(status().isOk());

        verify(service).pickup(1L, 1L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictUponUnknownCabIdExceptionDuringPickup() throws Exception {
        doThrow(new UnknownCabIdException()).when(service).pickup(2L, 3L);

        mockMvc.perform(post("/api/ec/requestPickup")
                .param("cabId", "2")
                .param("customerId", "3"))
                .andExpect(status().isConflict());

        verify(service).pickup(2L, 3L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictUponUnknownCabLocationExceptionDuringPickup() throws Exception {
        doThrow(new UnknownCabLocationException()).when(service).pickup(2L, 3L);

        mockMvc.perform(post("/api/ec/requestPickup")
                .param("cabId", "2")
                .param("customerId", "3"))
                .andExpect(status().isConflict());

        verify(service).pickup(2L, 3L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictUponUnknownJobIdExceptionDuringPickup() throws Exception {
        doThrow(new UnknownJobIdException()).when(service).pickup(2L, 3L);

        mockMvc.perform(post("/api/ec/requestPickup")
                .param("cabId", "2")
                .param("customerId", "3"))
                .andExpect(status().isConflict());

        verify(service).pickup(2L, 3L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictUponCabCustomerPositionConflictExceptionDuringPickup() throws Exception {
        doThrow(new CabCustomerPositionConflictException()).when(service).pickup(2L, 3L);

        mockMvc.perform(post("/api/ec/requestPickup")
                .param("cabId", "2")
                .param("customerId", "3"))
                .andExpect(status().isConflict());

        verify(service).pickup(2L, 3L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn400BadRequestUponIllegalArgumentExceptionDuringPickup() throws Exception {
        doThrow(new IllegalArgumentException()).when(service).pickup(2L, 3L);

        mockMvc.perform(post("/api/ec/requestPickup")
                .param("cabId", "2")
                .param("customerId", "3"))
                .andExpect(status().isBadRequest());

        verify(service).pickup(2L, 3L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturnPickupsComplete() throws Exception {
        when(service.pickupsComplete(1L)).thenReturn(PickupCompleteModel.builder()
                .complete(true)
                .build());

        mockMvc.perform(get("/api/ec/pickupsComplete")
                .param("cabId", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.completed").value("true"));

        verify(service).pickupsComplete(1L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictUponUnknownCabIdExceptionOnPickupsComplete() throws Exception {
        doThrow(new UnknownCabIdException()).when(service).pickupsComplete(2L);

        mockMvc.perform(get("/api/ec/pickupsComplete")
                .param("cabId", "2"))
                .andExpect(status().isConflict());

        verify(service).pickupsComplete(2L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn400BadRequestUponIllegalArgumentExceptionOnPickupsComplete() throws Exception {
        doThrow(new IllegalArgumentException()).when(service).pickupsComplete(2L);

        mockMvc.perform(get("/api/ec/pickupsComplete")
                .param("cabId", "2"))
                .andExpect(status().isBadRequest());

        verify(service).pickupsComplete(2L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturnPickupRequests() throws Exception {
        when(service.getPickupRequests()).thenReturn(asList(
                PickupRequestEntity.builder()
                        .cabId(1L)
                        .customerId(1L)
                        .build(),
                PickupRequestEntity.builder()
                        .cabId(2L)
                        .customerId(2L)
                        .build()));

        mockMvc.perform(get("/api/bookr/pickupRequests"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$[0].cabId").value(1))
                .andExpect(jsonPath("$[0].customerId").value(1))
                .andExpect(jsonPath("$[1].cabId").value(2))
                .andExpect(jsonPath("$[1].customerId").value(2));

        verify(service).getPickupRequests();
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturnEmptyListWhenPickupRequestsReturnsEmptyList() throws Exception {
        when(service.getPickupRequests()).thenReturn(emptyList());

        MvcResult mvcResult = mockMvc.perform(get("/api/bookr/pickupRequests"))
                .andExpect(status().isOk())
                .andReturn();

        assertThat(mvcResult.getResponse().getContentAsString())
                .isEqualToIgnoringWhitespace("[]");

        verify(service).getPickupRequests();
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldAcceptPickup() throws Exception {

        mockMvc.perform(post("/api/bookr/acceptPickup")
                .param("customerId", "1"))
                .andExpect(status().isOk());

        verify(service).acceptPickup(1L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictUponUnknownJobIdExceptionDuringAcceptPickup() throws Exception {
        doThrow(new UnknownJobIdException()).when(service).acceptPickup(2L);

        mockMvc.perform(post("/api/bookr/acceptPickup")
                .param("customerId", "2"))
                .andExpect(status().isConflict());

        verify(service).acceptPickup(2L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn400BadRequestUponIllegalArgumentExceptionDuringAcceptPickup() throws Exception {
        doThrow(new IllegalArgumentException()).when(service).acceptPickup(2L);

        mockMvc.perform(post("/api/bookr/acceptPickup")
                .param("customerId", "2"))
                .andExpect(status().isBadRequest());

        verify(service).acceptPickup(2L);
        verifyNoMoreInteractions(service);
    }
}
