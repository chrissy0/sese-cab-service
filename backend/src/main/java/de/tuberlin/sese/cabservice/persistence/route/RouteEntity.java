package de.tuberlin.sese.cabservice.persistence.route;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.List;
import java.util.Objects;

import static com.fasterxml.jackson.annotation.JsonInclude.Include.NON_NULL;

@Entity
@Builder
@Getter
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(NON_NULL)
public class RouteEntity {

    @Id
    private Long cabId;
    private Integer version;
    private Long jobId;
    private Long jobId2;
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @JsonProperty("route")
    private List<RouteActionEntity> routeActions;

    /**
     * Makes sure jobId2 is only set if jobId is already set
     */
    public RouteEntity setJobIds(Long... ids) {
        if (ids.length > 2) {
            throw new IllegalArgumentException("maximum of 2 jobs allowed");
        }

        this.jobId = null;
        this.jobId2 = null;

        int count = 0;
        for (Long id : ids) {
            if (count == 0) {
                this.jobId = id;
            }
            if (count == 1) {
                this.jobId2 = id;
            }
            count++;
        }
        return this;
    }

    /**
     * Returns true if this route is a subroute of the route passed as a parameter.
     * A subroute's actions have to equal the last length(superRoute) actions of the superRoute.
     */
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

    public void setCabId(Long cabId) {
        this.cabId = cabId;
    }

    public void setVersion(Integer version) {
        this.version = version;
    }

    private void setJobId(Long id) {
        // prevents setting jobId via setter
    }

    private void setJobId2(Long id) {
        // prevents setting jobId2 via setter
    }
}
