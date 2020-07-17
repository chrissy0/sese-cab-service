import {Sensor} from './sensor';

export class CabSensors {
  constructor(
    public cabId?: number,
    public sensors?: Sensor[],
  ) {
  }
}
