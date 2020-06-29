import {CustomerState} from './customer-state';

export class Job {
  constructor(
      public id?: number,
      public start?: number,
      public end?: number,
      public inProgress?: boolean,
      public customerState?: CustomerState,
  ) {
  }
}
