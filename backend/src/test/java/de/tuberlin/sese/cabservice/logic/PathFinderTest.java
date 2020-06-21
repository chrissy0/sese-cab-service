package de.tuberlin.sese.cabservice.logic;

import de.tuberlin.sese.cabservice.persistence.cab.blocked.CabBlockedService;
import de.tuberlin.sese.cabservice.util.exceptions.NoPathException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.List;

import static com.google.common.primitives.Ints.asList;
import static de.tuberlin.sese.cabservice.logic.Option.Direction.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.when;

@RunWith(SpringRunner.class)
@SpringBootTest
public class PathFinderTest {

    @Autowired
    private PathFinder pathFinder;

    @MockBean
    private CabBlockedService blockedService;

    @Test
    public void shouldFindShortestPath_between0And15() throws Exception {
        List<Option> route = pathFinder.getRouteBetween(0, 15);

        assertThat(route.get(0).getToSection()).isEqualTo(13);
        assertThat(route.get(0).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(1).getToSection()).isEqualTo(1);
        assertThat(route.get(1).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(2).getToSection()).isEqualTo(3);
        assertThat(route.get(2).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(3).getToSection()).isEqualTo(4);
        assertThat(route.get(3).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(4).getToSection()).isEqualTo(6);
        assertThat(route.get(4).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(5).getToSection()).isEqualTo(7);
        assertThat(route.get(5).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(6).getToSection()).isEqualTo(9);
        assertThat(route.get(6).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(7).getToSection()).isEqualTo(10);
        assertThat(route.get(7).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(8).getToSection()).isEqualTo(12);
        assertThat(route.get(8).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(9).getToSection()).isEqualTo(14);
        assertThat(route.get(9).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(10).getToSection()).isEqualTo(15);
        assertThat(route.get(10).getDirection()).isEqualTo(LEFT);
    }

    @Test
    public void shouldFindShortestPathWithBlockedSection_between0And15() throws Exception {
        when(blockedService.getBlockedSections()).thenReturn(asList(6, 9));

        List<Option> route = pathFinder.getRouteBetween(0, 15);

        assertThat(route.get(0).getToSection()).isEqualTo(13);
        assertThat(route.get(0).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(1).getToSection()).isEqualTo(1);
        assertThat(route.get(1).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(2).getToSection()).isEqualTo(3);
        assertThat(route.get(2).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(3).getToSection()).isEqualTo(4);
        assertThat(route.get(3).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(4).getToSection()).isEqualTo(5);
        assertThat(route.get(4).getDirection()).isEqualTo(RIGHT);

        assertThat(route.get(5).getToSection()).isEqualTo(7);
        assertThat(route.get(5).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(6).getToSection()).isEqualTo(8);
        assertThat(route.get(6).getDirection()).isEqualTo(RIGHT);

        assertThat(route.get(7).getToSection()).isEqualTo(10);
        assertThat(route.get(7).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(8).getToSection()).isEqualTo(12);
        assertThat(route.get(8).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(9).getToSection()).isEqualTo(14);
        assertThat(route.get(9).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(10).getToSection()).isEqualTo(15);
        assertThat(route.get(10).getDirection()).isEqualTo(LEFT);
    }

    @Test
    public void shouldThrowNoPathExceptionOnBlockedSections() {
        when(blockedService.getBlockedSections()).thenReturn(asList(5, 6));

        assertThatThrownBy(() -> pathFinder.getRouteBetween(4, 7))
                .isExactlyInstanceOf(NoPathException.class);
    }
}
