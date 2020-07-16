package de.tuberlin.sese.cabservice.persistence.customerinteraction.dropoff;

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
@WebMvcTest(controllers = DropoffController.class)
public class DropoffControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private DropoffService service;

    @Test
    public void shouldRequestDropoff() throws Exception {

        mockMvc.perform(post("/api/ec/requestDropoff")
                .param("cabId", "1")
                .param("customerId", "1"))
                .andExpect(status().isOk());

        verify(service).dropoff(1L, 1L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictUponUnknownCabIdExceptionDuringDropoff() throws Exception {
        doThrow(new UnknownCabIdException()).when(service).dropoff(2L, 3L);

        mockMvc.perform(post("/api/ec/requestDropoff")
                .param("cabId", "2")
                .param("customerId", "3"))
                .andExpect(status().isConflict());

        verify(service).dropoff(2L, 3L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictUponUnknownCabLocationExceptionDuringDropoff() throws Exception {
        doThrow(new UnknownCabLocationException()).when(service).dropoff(2L, 3L);

        mockMvc.perform(post("/api/ec/requestDropoff")
                .param("cabId", "2")
                .param("customerId", "3"))
                .andExpect(status().isConflict());

        verify(service).dropoff(2L, 3L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictUponUnknownJobIdExceptionDuringDropoff() throws Exception {
        doThrow(new UnknownJobIdException()).when(service).dropoff(2L, 3L);

        mockMvc.perform(post("/api/ec/requestDropoff")
                .param("cabId", "2")
                .param("customerId", "3"))
                .andExpect(status().isConflict());

        verify(service).dropoff(2L, 3L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictUponCabCustomerPositionConflictExceptionDuringDropoff() throws Exception {
        doThrow(new CabCustomerPositionConflictException()).when(service).dropoff(2L, 3L);

        mockMvc.perform(post("/api/ec/requestDropoff")
                .param("cabId", "2")
                .param("customerId", "3"))
                .andExpect(status().isConflict());

        verify(service).dropoff(2L, 3L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn400BadRequestUponIllegalArgumentExceptionDuringDropoff() throws Exception {
        doThrow(new IllegalArgumentException()).when(service).dropoff(2L, 3L);

        mockMvc.perform(post("/api/ec/requestDropoff")
                .param("cabId", "2")
                .param("customerId", "3"))
                .andExpect(status().isBadRequest());

        verify(service).dropoff(2L, 3L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturnDropoffsComplete() throws Exception {
        when(service.dropoffsComplete(1L)).thenReturn(DropoffCompleteModel.builder()
                .complete(true)
                .build());

        mockMvc.perform(get("/api/ec/dropoffsComplete")
                .param("cabId", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.completed").value("true"));

        verify(service).dropoffsComplete(1L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictUponUnknownCabIdExceptionOnDropoffsComplete() throws Exception {
        doThrow(new UnknownCabIdException()).when(service).dropoffsComplete(2L);

        mockMvc.perform(get("/api/ec/dropoffsComplete")
                .param("cabId", "2"))
                .andExpect(status().isConflict());

        verify(service).dropoffsComplete(2L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn400BadRequestUponIllegalArgumentExceptionOnDropoffsComplete() throws Exception {
        doThrow(new IllegalArgumentException()).when(service).dropoffsComplete(2L);

        mockMvc.perform(get("/api/ec/dropoffsComplete")
                .param("cabId", "2"))
                .andExpect(status().isBadRequest());

        verify(service).dropoffsComplete(2L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturnDropoffRequests() throws Exception {
        when(service.getDropoffRequests()).thenReturn(asList(
                DropoffRequestEntity.builder()
                        .cabId(1L)
                        .customerId(1L)
                        .build(),
                DropoffRequestEntity.builder()
                        .cabId(2L)
                        .customerId(2L)
                        .build()));

        mockMvc.perform(get("/api/bookr/dropoffRequests"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$[0].cabId").value(1))
                .andExpect(jsonPath("$[0].customerId").value(1))
                .andExpect(jsonPath("$[1].cabId").value(2))
                .andExpect(jsonPath("$[1].customerId").value(2));

        verify(service).getDropoffRequests();
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturnEmptyListWhenDropoffRequestsReturnsEmptyList() throws Exception {
        when(service.getDropoffRequests()).thenReturn(emptyList());

        MvcResult mvcResult = mockMvc.perform(get("/api/bookr/dropoffRequests"))
                .andExpect(status().isOk())
                .andReturn();

        assertThat(mvcResult.getResponse().getContentAsString())
                .isEqualToIgnoringWhitespace("[]");

        verify(service).getDropoffRequests();
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldAcceptDropoff() throws Exception {

        mockMvc.perform(post("/api/bookr/acceptDropoff")
                .param("customerId", "1"))
                .andExpect(status().isOk());

        verify(service).acceptDropoff(1L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictUponUnknownJobIdExceptionDuringAcceptDropoff() throws Exception {
        doThrow(new UnknownJobIdException()).when(service).acceptDropoff(2L);

        mockMvc.perform(post("/api/bookr/acceptDropoff")
                .param("customerId", "2"))
                .andExpect(status().isConflict());

        verify(service).acceptDropoff(2L);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn400BadRequestUponIllegalArgumentExceptionDuringAcceptDropoff() throws Exception {
        doThrow(new IllegalArgumentException()).when(service).acceptDropoff(2L);

        mockMvc.perform(post("/api/bookr/acceptDropoff")
                .param("customerId", "2"))
                .andExpect(status().isBadRequest());

        verify(service).acceptDropoff(2L);
        verifyNoMoreInteractions(service);
    }
}
