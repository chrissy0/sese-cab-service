import {RouteAction} from './route-action';

export class Route {
  constructor(
      public cabId?: number,
      public version?: number,
      public jobId?: number,
      public jobId2?: number,
      public route?: RouteAction[],
  ) {
  }
}
