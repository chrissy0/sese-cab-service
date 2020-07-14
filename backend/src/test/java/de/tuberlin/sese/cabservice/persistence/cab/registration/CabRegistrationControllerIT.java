package de.tuberlin.sese.cabservice.persistence.cab.registration;

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
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@SpringBootTest
@DirtiesContext(classMode = BEFORE_EACH_TEST_METHOD)
public class CabRegistrationControllerIT {

    @Autowired
    private WebApplicationContext wac;

    private MockMvc mockMvc;

    @Before
    public void setUp() {
        this.mockMvc = MockMvcBuilders.webAppContextSetup(this.wac).build();
    }

    @Test
    public void shouldRegisterCab() throws Exception {
        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabName\": \"Some Cab Name\", \"section\": 3}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(1));
    }

    @Test
    public void shouldReturn409ConflictIfNameAlreadyInUse() throws Exception {
        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabName\": \"Some Cab Name\", \"section\": 3}"))
                .andExpect(status().isOk());

        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabName\": \"Some Cab Name\", \"section\": 1}"))
                .andExpect(status().isConflict());
    }

    @Test
    public void shouldReturn409ConflictIfSectionIsUnknown() throws Exception {
        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabName\": \"Some Cab Name\", \"section\": 16}"))
                .andExpect(status().isConflict());
    }

    @Test
    public void shouldReturn409ConflictIfRequestIsMalformed() throws Exception {
        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabNAm\": \"Some Cab Name\", \"section\": 16}"))
                .andExpect(status().isBadRequest());
    }
}
