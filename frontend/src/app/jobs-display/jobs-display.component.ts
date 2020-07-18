import {Component, OnInit} from '@angular/core';
import {Job} from '../job';
import {BackendService} from '../backend.service';
import {faTrashAlt} from '@fortawesome/free-regular-svg-icons/faTrashAlt';
import {Pickup} from '../pickup';
import {Dropoff} from '../dropoff';
import {CustomerState} from '../customer-state';
import {faTaxi} from '@fortawesome/free-solid-svg-icons';

@Component({
  selector: 'app-jobs-display',
  templateUrl: './jobs-display.component.html',
  styleUrls: ['./jobs-display.component.css']
})
export class JobsDisplayComponent implements OnInit {

  constructor(private backendService: BackendService) {
  }

  // all currently available jobs
  jobs: Job[] = [];

  // all currently pending pickups
  pickups: Pickup[] = [];

  // all currently pending dropoffs
  dropoffs: Dropoff[] = [];

  taxiIcon = faTaxi;
  trashIcon = faTrashAlt;

  // returns all currently pending pickups for a specific job
  pickupForJob(jobId: number) {
    const pickupsFiltered = this.pickups.filter(pickup => pickup.customerId === jobId);
    if (pickupsFiltered.length > 0) {
      return pickupsFiltered[0];
    }
    return undefined;
  }

  // returns all currently pending dropoffs for a specific job
  dropoffForJob(jobId: number) {
    const dropoffsFiltered = this.dropoffs.filter(dropoff => dropoff.customerId === jobId);
    if (dropoffsFiltered.length > 0) {
      return dropoffsFiltered[0];
    }
    return undefined;
  }

  // updates job table including jobs, pickups and dropoffs
  updateTable() {
    this.backendService.getJobs().then(jobs => {
      this.jobs = [];
      jobs.forEach(job => this.jobs.push(job));
    });
    this.backendService.getPickups().then(pickups => {
      this.pickups = [];
      pickups.forEach(pickup => this.pickups.push(pickup));
    });
    this.backendService.getDropoffs().then(dropoffs => {
      this.dropoffs = [];
      dropoffs.forEach(dropoff => this.dropoffs.push(dropoff));
    });
  }

  deleteJob(tableIndex: number) {
    this.backendService.deleteJob(this.jobs[tableIndex].id);
  }

  deleteJobButtonEnabled(job: Job) {
    return CustomerState.WAITING === job.customerState;
  }

  // returns true if there is at least one active job
  jobsActive() {
    return this.jobs.length > 0;
  }

  enterCab(customerId: number) {
    this.backendService.acceptPickup(customerId);
  }

  exitCab(customerId: number) {
    this.backendService.acceptDropoff(customerId);
  }

  // updates job table every second
  ngOnInit(): void {
    this.updateTable();
    setInterval(() => {
      this.updateTable();
    }, 1000);
  }

}
