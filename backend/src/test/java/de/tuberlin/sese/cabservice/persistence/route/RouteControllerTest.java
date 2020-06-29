package de.tuberlin.sese.cabservice.persistence.route;

import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.VersionException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import static de.tuberlin.sese.cabservice.persistence.route.RouteActionEntity.Action.*;
import static de.tuberlin.sese.cabservice.persistence.route.RouteActionEntity.Direction.LEFT;
import static de.tuberlin.sese.cabservice.persistence.route.RouteActionEntity.Direction.RIGHT;
import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = RouteController.class)
public class RouteControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private RouteService service;

    @Test
    public void shouldGetRoute() throws Exception {
        when(service.getRoute(1L, 0)).thenReturn(RouteEntity.builder()
                .version(0)
                .routeActions(asList(
                        RouteActionEntity.builder()
                                .action(TURN)
                                .direction(RIGHT)
                                .marker(1)
                                .build(),
                        RouteActionEntity.builder()
                                .action(TURN)
                                .direction(LEFT)
                                .marker(3)
                                .build(),
                        RouteActionEntity.builder()
                                .action(PICKUP)
                                .customerId(0L)
                                .marker(2)
                                .build(),
                        RouteActionEntity.builder()
                                .action(DROPOFF)
                                .customerId(0L)
                                .marker(4)
                                .build(),
                        RouteActionEntity.builder()
                                .action(WAIT)
                                .marker(0)
                                .build()))
                .build());

        MvcResult result = mockMvc.perform(get("/api/ec/requestRoute")
                .param("id", "1")
                .param("version", "0"))
                .andExpect(status().isOk())
                .andReturn();

        assertThat(result.getResponse().getContentAsString())
                .isEqualToIgnoringWhitespace("" +
                        "{\n" +
                        "   \"version\":0,\n" +
                        "   \"route\":[\n" +
                        "      {\n" +
                        "         \"action\":\"turn\",\n" +
                        "         \"direction\":\"right\",\n" +
                        "         \"marker\":1\n" +
                        "      },\n" +
                        "      {\n" +
                        "         \"action\":\"turn\",\n" +
                        "         \"direction\":\"left\",\n" +
                        "         \"marker\":3\n" +
                        "      },\n" +
                        "      {\n" +
                        "         \"action\":\"pickup\",\n" +
                        "         \"marker\":2,\n" +
                        "         \"customerId\":0\n" +
                        "      },\n" +
                        "      {\n" +
                        "         \"action\":\"dropoff\",\n" +
                        "         \"marker\":4,\n" +
                        "         \"customerId\":0\n" +
                        "      },\n" +
                        "      {\n" +
                        "         \"action\":\"wait\",\n" +
                        "         \"marker\":0\n" +
                        "      }\n" +
                        "   ]\n" +
                        "}");

        verify(service).getRoute(1L, 0);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldGetVersionOnly() throws Exception {
        when(service.getRoute(1L, 0)).thenReturn(RouteEntity.builder()
                .version(0)
                .build());

        MvcResult result = mockMvc.perform(get("/api/ec/requestRoute")
                .param("id", "1")
                .param("version", "0"))
                .andExpect(status().isOk())
                .andReturn();

        assertThat(result.getResponse().getContentAsString())
                .isEqualToIgnoringWhitespace("" +
                        "{\n" +
                        "   \"version\":0\n" +
                        "}");

        verify(service).getRoute(1L, 0);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictOnVersionException() throws Exception {
        doThrow(new VersionException()).when(service).getRoute(12L, 0);

        mockMvc.perform(get("/api/ec/requestRoute")
                .param("id", "12")
                .param("version", "0"))
                .andExpect(status().isConflict());

        verify(service).getRoute(12L, 0);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn409ConflictOnUnknownCabIdException() throws Exception {
        doThrow(new UnknownCabIdException()).when(service).getRoute(12L, 0);

        mockMvc.perform(get("/api/ec/requestRoute")
                .param("id", "12")
                .param("version", "0"))
                .andExpect(status().isConflict());

        verify(service).getRoute(12L, 0);
        verifyNoMoreInteractions(service);
    }

    @Test
    public void shouldReturn500InternalServerErrorOnIllegalStateException() throws Exception {
        doThrow(new IllegalStateException()).when(service).getRoute(12L, 0);

        mockMvc.perform(get("/api/ec/requestRoute")
                .param("id", "12")
                .param("version", "0"))
                .andExpect(status().isInternalServerError());

        verify(service).getRoute(12L, 0);
        verifyNoMoreInteractions(service);
    }
}
