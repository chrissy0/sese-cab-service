package de.tuberlin.sese.cabservice.persistence.route;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.junit4.SpringRunner;

import static de.tuberlin.sese.cabservice.persistence.route.RouteActionEntity.Action.PICKUP;
import static de.tuberlin.sese.cabservice.persistence.route.RouteActionEntity.Action.TURN;
import static de.tuberlin.sese.cabservice.persistence.route.RouteActionEntity.Direction.RIGHT;
import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.annotation.DirtiesContext.ClassMode.BEFORE_EACH_TEST_METHOD;

@RunWith(SpringRunner.class)
@SpringBootTest
@DirtiesContext(classMode = BEFORE_EACH_TEST_METHOD)
public class RouteRepoTest {

    @Autowired
    private RouteRepo repo;

    @Test
    public void shouldSaveRouteIncludingRouteActions() {
        RouteActionEntity routeAction1 = RouteActionEntity.builder()
                .action(TURN)
                .direction(RIGHT)
                .marker(14)
                .build();
        RouteActionEntity routeAction2 = RouteActionEntity.builder()
                .action(PICKUP)
                .customerId(2)
                .marker(7)
                .build();

        RouteEntity entity = RouteEntity.builder()
                .cabId(2L)
                .version(3)
                .routeActions(asList(routeAction1, routeAction2))
                .build();

        repo.save(entity);

        RouteEntity loadedEntity = repo.findAll().iterator().next();

        assertThat(loadedEntity).isNotNull();
        assertThat(loadedEntity.getCabId()).isEqualTo(2);
        assertThat(loadedEntity.getVersion()).isEqualTo(3);

        RouteActionEntity firstAction = loadedEntity.getRouteActions().get(0);
        RouteActionEntity secondAction = loadedEntity.getRouteActions().get(1);

        assertThat(firstAction.getAction()).isEqualTo(TURN);
        assertThat(firstAction.getDirection()).isEqualTo(RIGHT);
        assertThat(firstAction.getMarker()).isEqualTo(14);
        assertThat(firstAction.getCustomerId()).isNull();
        assertThat(firstAction.getId()).isNotNull();

        assertThat(secondAction.getAction()).isEqualTo(PICKUP);
        assertThat(secondAction.getCustomerId()).isEqualTo(2);
        assertThat(secondAction.getMarker()).isEqualTo(7);
        assertThat(secondAction.getDirection()).isNull();
        assertThat(secondAction.getId()).isNotNull();
    }
}
