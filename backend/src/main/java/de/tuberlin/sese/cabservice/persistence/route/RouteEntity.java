package de.tuberlin.sese.cabservice.persistence.route;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.persistence.*;
import java.util.List;
import java.util.Objects;

import static com.fasterxml.jackson.annotation.JsonInclude.Include.NON_NULL;

@Entity
@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(NON_NULL)
public class RouteEntity {

    @Id
    private Long cabId;
    private Integer version;
    @JsonIgnore
    private Long jobId;
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @JsonProperty("route")
    private List<RouteActionEntity> routeActions;

    public boolean isSubRouteOf(RouteEntity superRoute) {
        List<RouteActionEntity> superRouteActions = superRoute.getRouteActions();
        if (superRouteActions.size() < routeActions.size()) {
            return false;
        }
        int sizeDifference = superRouteActions.size() - routeActions.size();
        for (int i = 0; i < routeActions.size(); i++) {
            RouteActionEntity subRouteAction = routeActions.get(i);
            RouteActionEntity superRouteAction = superRouteActions.get(i + sizeDifference);

            if (subRouteAction.getAction() != superRouteAction.getAction()) {
                return false;
            }
            if (!java.util.Objects.equals(subRouteAction.getMarker(), superRouteAction.getMarker())) {
                return false;
            }
            if (subRouteAction.getDirection() != superRouteAction.getDirection()) {
                return false;
            }
            if (!java.util.Objects.equals(subRouteAction.getCustomerId(), superRouteAction.getCustomerId())) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        return Objects.hash(routeActions);
    }
}
