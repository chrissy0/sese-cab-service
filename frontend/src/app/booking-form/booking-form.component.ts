import {Component} from '@angular/core';
import {Job} from '../job';
import {BackendService} from '../backend.service';

@Component({
  selector: 'app-booking-form',
  templateUrl: './booking-form.component.html',
  styleUrls: ['./booking-form.component.css']
})
export class BookingFormComponent {

  constructor(private backendService: BackendService) {
  }

  stations = [2, 5, 8, 11];

  model = new Job();

  pictureCollapsed = true;

  submitButtonDisabled() {
    return this.model.start === undefined || this.model.end === undefined;
  }

  onSubmit() {
    this.backendService.saveJob(this.model).then(() => this.model = new Job());
  }

  getStartStations() {
    // tslint:disable-next-line:triple-equals
    return this.stations.filter(station => station != this.model.end);
  }

  getEndStations() {
    // tslint:disable-next-line:triple-equals
    return this.stations.filter(station => station != this.model.start);
  }

}
