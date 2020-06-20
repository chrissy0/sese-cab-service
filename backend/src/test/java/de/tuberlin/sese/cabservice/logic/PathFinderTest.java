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
import java.util.Optional;

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
        Optional<List<Option>> routeOptional = pathFinder.getRouteBetween(0, 15);
        assertThat(routeOptional).isPresent();

        List<Option> route = routeOptional.get();

        assertThat(route.get(0).getSection()).isEqualTo(13);
        assertThat(route.get(0).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(1).getSection()).isEqualTo(1);
        assertThat(route.get(1).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(2).getSection()).isEqualTo(3);
        assertThat(route.get(2).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(3).getSection()).isEqualTo(4);
        assertThat(route.get(3).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(4).getSection()).isEqualTo(6);
        assertThat(route.get(4).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(5).getSection()).isEqualTo(7);
        assertThat(route.get(5).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(6).getSection()).isEqualTo(9);
        assertThat(route.get(6).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(7).getSection()).isEqualTo(10);
        assertThat(route.get(7).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(8).getSection()).isEqualTo(12);
        assertThat(route.get(8).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(9).getSection()).isEqualTo(14);
        assertThat(route.get(9).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(10).getSection()).isEqualTo(15);
        assertThat(route.get(10).getDirection()).isEqualTo(LEFT);
    }

    @Test
    public void shouldFindShortestPathWithBlockedSection_between0And15() throws Exception {
        when(blockedService.getBlockedSections()).thenReturn(asList(6, 9));

        Optional<List<Option>> routeOptional = pathFinder.getRouteBetween(0, 15);
        assertThat(routeOptional).isPresent();

        List<Option> route = routeOptional.get();

        assertThat(route.get(0).getSection()).isEqualTo(13);
        assertThat(route.get(0).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(1).getSection()).isEqualTo(1);
        assertThat(route.get(1).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(2).getSection()).isEqualTo(3);
        assertThat(route.get(2).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(3).getSection()).isEqualTo(4);
        assertThat(route.get(3).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(4).getSection()).isEqualTo(5);
        assertThat(route.get(4).getDirection()).isEqualTo(RIGHT);

        assertThat(route.get(5).getSection()).isEqualTo(7);
        assertThat(route.get(5).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(6).getSection()).isEqualTo(8);
        assertThat(route.get(6).getDirection()).isEqualTo(RIGHT);

        assertThat(route.get(7).getSection()).isEqualTo(10);
        assertThat(route.get(7).getDirection()).isEqualTo(STRAIGHT);

        assertThat(route.get(8).getSection()).isEqualTo(12);
        assertThat(route.get(8).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(9).getSection()).isEqualTo(14);
        assertThat(route.get(9).getDirection()).isEqualTo(LEFT);

        assertThat(route.get(10).getSection()).isEqualTo(15);
        assertThat(route.get(10).getDirection()).isEqualTo(LEFT);
    }

    @Test
    public void shouldThrowNoPathExceptionOnBlockedSections() {
        when(blockedService.getBlockedSections()).thenReturn(asList(5, 6));

        assertThatThrownBy(() -> pathFinder.getRouteBetween(4, 7))
                .isExactlyInstanceOf(NoPathException.class);
    }
}
