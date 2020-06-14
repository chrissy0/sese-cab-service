package de.tuberlin.sese.cabservice.cab.blocked;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.annotation.DirtiesContext.ClassMode.BEFORE_EACH_TEST_METHOD;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@SpringBootTest
@DirtiesContext(classMode = BEFORE_EACH_TEST_METHOD)
public class CabBlockedControllerIT {

    @Autowired
    private WebApplicationContext wac;

    private MockMvc mockMvc;

    @Before
    public void setUp() {
        this.mockMvc = MockMvcBuilders.webAppContextSetup(this.wac).build();
    }

    @Test
    public void shouldSetAndRemoveCabBlocked() throws Exception {
        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabName\": \"Some Cab Name\", \"section\": 1}"))
                .andExpect(status().isOk());

        mockMvc.perform(post("/api/ec/blocked")
                .param("cabId", "1")
                .param("blocked", "true"))
                .andExpect(status().isOk());

        mockMvc.perform(post("/api/ec/blocked")
                .param("cabId", "1")
                .param("blocked", "false"))
                .andExpect(status().isOk());
    }

    @Test
    public void shouldSetCabBlockedIfAlreadyBlockedWithoutError() throws Exception {
        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabName\": \"Some Cab Name\", \"section\": 1}"))
                .andExpect(status().isOk());

        mockMvc.perform(post("/api/ec/blocked")
                .param("cabId", "1")
                .param("blocked", "true"))
                .andExpect(status().isOk());

        mockMvc.perform(post("/api/ec/blocked")
                .param("cabId", "1")
                .param("blocked", "true"))
                .andExpect(status().isOk());
    }

    @Test
    public void shouldSetCabUnblockedIfAlreadyUnblockedWithoutError() throws Exception {
        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabName\": \"Some Cab Name\", \"section\": 1}"))
                .andExpect(status().isOk());

        mockMvc.perform(post("/api/ec/blocked")
                .param("cabId", "1")
                .param("blocked", "false"))
                .andExpect(status().isOk());
    }

    @Test
    public void shouldReturn409ConflictOnUnknownCabId() throws Exception {
        mockMvc.perform(post("/api/ec/blocked")
                .param("cabId", "1")
                .param("blocked", "true"))
                .andExpect(status().isConflict());
    }

    @Test
    public void shouldReturn400BadRequestOnMissingCabId() throws Exception {
        mockMvc.perform(post("/api/ec/blocked")
                .param("blocked", "true"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void shouldReturn400BadRequestOnMissingBlocked() throws Exception {
        mockMvc.perform(post("/api/ec/blocked")
                .param("cabId", "1"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void shouldReturn400BadRequestOnMalformedCabId() throws Exception {
        mockMvc.perform(post("/api/ec/blocked")
                .param("cabId", "abc")
                .param("blocked", "true"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void shouldReturn400BadRequestOnMalformedBlocked() throws Exception {
        mockMvc.perform(post("/api/ec/blocked")
                .param("cabId", "1")
                .param("blocked", "tru"))
                .andExpect(status().isBadRequest());
    }
}
