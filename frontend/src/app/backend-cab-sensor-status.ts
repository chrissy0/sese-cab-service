import {BackendSensorStatus} from './backend-sensor-status';

export class BackendCabSensorStatus {
  constructor(
    public sensors?: BackendSensorStatus[],
  ) {
  }
}
