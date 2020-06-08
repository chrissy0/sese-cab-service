import {Component} from '@angular/core';
import {Job} from "./job";

@Component({
  selector: 'app-booking-form',
  templateUrl: './booking-form.component.html',
  styleUrls: ['./booking-form.component.css']
})
export class BookingFormComponent {

  stations = [10, 11, 12, 13];

  model = new Job();

  onSubmit() {
    this.model = new Job()
  }

  getStartStations() {
    return this.stations.filter(station => station != this.model.end);
  }

  getEndStations() {
    return this.stations.filter(station => station != this.model.start);
  }

}
