package de.tuberlin.sese.cabservice.persistence.debug;

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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@SpringBootTest
@DirtiesContext(classMode = BEFORE_EACH_TEST_METHOD)
public class DebugControllerIT {

    @Autowired
    private WebApplicationContext wac;

    private MockMvc mockMvc;

    @Before
    public void setUp() {
        this.mockMvc = MockMvcBuilders.webAppContextSetup(this.wac).build();
    }

    @Test
    public void shouldReturnCabSensorStatus() throws Exception {
        mockMvc.perform(post("/api/ec/registerCab")
                .contentType(APPLICATION_JSON)
                .content("{\"cabName\": \"Some Cab Name\", \"section\": 1}"))
                .andExpect(status().isOk());

        mockMvc.perform(post("/api/bookr/setSensorStatus")
                .param("cabId", "1")
                .param("sensorName", "inf_rm_br")
                .param("disabled", "false")
                .param("noise", "13"))
                .andExpect(status().isOk());

        mockMvc.perform(post("/api/bookr/setSensorStatus")
                .param("cabId", "1")
                .param("sensorName", "inf_rm_br2")
                .param("disabled", "true")
                .param("noise", "0"))
                .andExpect(status().isOk());

        mockMvc.perform(get("/api/ec/sensorStatus")
                .param("cabId", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.sensorErrors").value(true))
                .andExpect(jsonPath("$.sensors[0].name").value("inf_rm_br"))
                .andExpect(jsonPath("$.sensors[0].disabled").value(false))
                .andExpect(jsonPath("$.sensors[0].whoosh").value(13))
                .andExpect(jsonPath("$.sensors[1].name").value("inf_rm_br2"))
                .andExpect(jsonPath("$.sensors[1].disabled").value(true))
                .andExpect(jsonPath("$.sensors[1].whoosh").value(0));

        mockMvc.perform(post("/api/bookr/setSensorStatus")
                .param("cabId", "1")
                .param("sensorName", "inf_rm_br2")
                .param("disabled", "false")
                .param("noise", "0"))
                .andExpect(status().isOk());

        mockMvc.perform(get("/api/ec/sensorStatus")
                .param("cabId", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.sensorErrors").value(true))
                .andExpect(jsonPath("$.sensors[0].name").value("inf_rm_br"))
                .andExpect(jsonPath("$.sensors[0].disabled").value(false))
                .andExpect(jsonPath("$.sensors[0].whoosh").value(13))
                .andExpect(jsonPath("$.sensors[1].name").doesNotExist())
                .andExpect(jsonPath("$.sensors[1].disabled").doesNotExist())
                .andExpect(jsonPath("$.sensors[1].whoosh").doesNotExist());

        mockMvc.perform(post("/api/bookr/setSensorStatus")
                .param("cabId", "1")
                .param("sensorName", "inf_rm_br")
                .param("disabled", "false")
                .param("noise", "0"))
                .andExpect(status().isOk());

        mockMvc.perform(get("/api/ec/sensorStatus")
                .param("cabId", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.sensorErrors").value(false))
                .andExpect(jsonPath("$.sensors").doesNotExist());
    }
}
