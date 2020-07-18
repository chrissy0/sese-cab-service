import {Component, OnInit} from '@angular/core';
import {Route} from '../route';
import {BackendService} from '../backend.service';
import {RouteAction} from '../route-action';
import {faBolt, faMinusCircle} from '@fortawesome/free-solid-svg-icons';

@Component({
  selector: 'app-routes-display',
  templateUrl: './routes-display.component.html',
  styleUrls: ['./routes-display.component.css']
})
export class RoutesDisplayComponent implements OnInit {

  constructor(private backendService: BackendService) {
  }

  minusCircleIcon = faMinusCircle;
  boltIcon = faBolt;

  // all currently active routes
  routes: Route[] = [];

  // cab ids of all currently blocked cabs
  blockedCabIds: number[] = [];

  // cab ids of all currently dysfunctional cabs
  dysfunctionalCabIds: number[] = [];

  // updates route table including routes, blocked cabs and dysfunctional cabs
  updateTable() {
    this.backendService.getRoutes().then(routes => {
      this.routes = [];
      routes.forEach(route => this.routes.push(route));
    });

    this.backendService.getBlockedCabIds().then(blockedCabIds => {
      this.blockedCabIds = [];
      blockedCabIds.forEach(blockedCabId => this.blockedCabIds.push(blockedCabId));
    });

    this.backendService.getDysfunctionalCabIds().then(dysfunctionalCabIds => {
      this.dysfunctionalCabIds = [];
      dysfunctionalCabIds.forEach(dysfunctionalCabId => this.dysfunctionalCabIds.push(dysfunctionalCabId));
    });
  }

  // returns true if there is at least one active route
  routesAvailable() {
    return this.routes.length > 0;
  }

  // formats job ids to display in frontend
  jobIdsString(route: Route) {
    if (route.jobId === undefined) {
      return '-';
    }
    if (route.jobId2 === undefined) {
      return route.jobId;
    }
    return route.jobId + ', ' + route.jobId2;
  }

  // returns string representation of route to display in frontend
  routeActionsString(routeActions: RouteAction[]) {
    let routeString = '';
    for (let i = 0; i < routeActions.length; i++) {
      const action = routeActions[i];
      routeString += '[';
      if (action.marker < 10) {
        routeString += ' ';
      }
      routeString += action.marker + '] ';
      if (action.action === 'pickup') {
        routeString += '↑ 🧔 ' + action.customerId;
      } else if (action.action === 'dropoff') {
        routeString += '↓ 🧔 ' + action.customerId;
      } else if (action.action === 'turn') {
        if (action.direction === 'right') {
          routeString += '→';
        }
        if (action.direction === 'left') {
          routeString += '←';
        }
      } else if (action.action === 'wait') {
        routeString += '↻';
      }
      if (i < routeActions.length - 1) {
        routeString += '<br>';
      }
    }
    return routeString;
  }

  // updates route table every second
  ngOnInit(): void {
    this.updateTable();
    setInterval(() => {
      this.updateTable();
    }, 1000);
  }

  cabIsBlocked(cabId: number) {
    for (const blockedCabId of this.blockedCabIds) {
      if (cabId === blockedCabId) {
        return true;
      }
    }
    return false;
  }

  cabIsDysfunctional(cabId: number) {
    for (const dysfunctionalCabId of this.dysfunctionalCabIds) {
      if (cabId === dysfunctionalCabId) {
        return true;
      }
    }
    return false;
  }
}
