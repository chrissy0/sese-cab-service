import {Component, OnInit} from '@angular/core';
import {BackendService} from '../backend.service';
import {Cab} from '../cab';
import {Sensor} from '../sensor';
import {CabSensors} from '../cab-sensors';

@Component({
  selector: 'app-debug',
  templateUrl: './debug.component.html',
  styleUrls: ['./debug.component.css']
})
export class DebugComponent implements OnInit {

  constructor(private backendService: BackendService) {
  }

  cabs: Cab[] = [];

  cabSensors: CabSensors[] = [];

  getCabSensors(cabId: number) {
    return this.cabSensors.filter(elem => elem.cabId === cabId)[0].sensors;
  }

  setNoise(cabId: number, sensor: Sensor, n: string) {
    sensor.noise = parseInt(n, 10);
    this.backendService.saveSensorData(cabId, sensor);
  }

  setActive(cabId: number, sensor: Sensor, active: boolean) {
    sensor.active = active;
    this.backendService.saveSensorData(cabId, sensor);
  }

  ngOnInit(): void {
    this.loadDebugPanel();
  }

  private loadDebugPanel() {
    this.backendService.getRegisteredCabs().then(cabs => {
      this.cabs = [];
      cabs.forEach(cab => {
        this.cabs.push(cab);

        const data = {
          cabId: cab.id,
          sensors: [
            {name: 'inf_right', active: true, noise: 0},
            {name: 'inf_right2', active: true, noise: 0},
            {name: 'inf_cent', active: true, noise: 0},
            {name: 'inf_cent2', active: true, noise: 0},
            {name: 'inf_rm_fr', active: true, noise: 0},
            {name: 'inf_rm_fr2', active: true, noise: 0},
            {name: 'inf_rm_fr_act', active: true, noise: 0},
            {name: 'inf_rm_fr_act2', active: true, noise: 0},
            {name: 'inf_rm_fl', active: true, noise: 0},
            {name: 'inf_rm_fl2', active: true, noise: 0},
            {name: 'inf_rm_fl_act', active: true, noise: 0},
            {name: 'inf_rm_fl_act2', active: true, noise: 0},
            {name: 'inf_rm_bl', active: true, noise: 0},
            {name: 'inf_rm_bl2', active: true, noise: 0},
            {name: 'inf_rm_bl_act', active: true, noise: 0},
            {name: 'inf_rm_bl_act2', active: true, noise: 0},
            {name: 'inf_rm_br', active: true, noise: 0},
            {name: 'inf_rm_br2', active: true, noise: 0},
            {name: 'inf_rm_br_act', active: true, noise: 0},
            {name: 'inf_rm_br_act2', active: true, noise: 0},
            {name: 'dist_c', active: true, noise: 0},
            {name: 'dist_c2', active: true, noise: 0},
            {name: 'curb_rb', active: true, noise: 0},
            {name: 'curb_rb2', active: true, noise: 0},
            {name: 'curb_rc', active: true, noise: 0},
            {name: 'curb_rc2', active: true, noise: 0},
            {name: 'curb_rf', active: true, noise: 0},
            {name: 'curb_rf2', active: true, noise: 0},
            {name: 'curb_lf', active: true, noise: 0},
            {name: 'curb_lf2', active: true, noise: 0},
            {name: 'curb_lc', active: true, noise: 0},
            {name: 'curb_lc2', active: true, noise: 0},
            {name: 'curb_lb', active: true, noise: 0},
            {name: 'curb_lb2', active: true, noise: 0},
            {name: 'dist_r', active: true, noise: 0},
            {name: 'dist_r2', active: true, noise: 0},
            {name: 'dist_l', active: true, noise: 0},
            {name: 'dist_l2', active: true, noise: 0},
            {name: 'inf_left', active: true, noise: 0},
            {name: 'inf_left2', active: true, noise: 0},
            {name: 'wheel1', active: true, noise: 0},
            {name: 'wheel2', active: true, noise: 0},
            {name: 'wheel3', active: true, noise: 0},
            {name: 'wheel4', active: true, noise: 0}
          ]
        };

        const sensorData = this.backendService.getSensorData(cab.id);
        sensorData.then(value => {
          if (value.sensors === undefined || value.sensors === null) {
            return;
          }
          value.sensors.forEach(newSensor => {
            for (const sensor of data.sensors) {
              if (sensor.name === newSensor.name) {
                sensor.active = !newSensor.disabled;
                sensor.noise = newSensor.whoosh;
              }
            }
          });
        });

        this.cabSensors.push(data);
      });
    });
  }
}
