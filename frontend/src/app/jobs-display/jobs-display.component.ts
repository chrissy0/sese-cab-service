import {Component, OnInit} from '@angular/core';
import {Job} from '../job';
import {BackendService} from '../backend.service';

@Component({
    selector: 'app-jobs-display',
    templateUrl: './jobs-display.component.html',
    styleUrls: ['./jobs-display.component.css']
})
export class JobsDisplayComponent implements OnInit {

    constructor(private backendService: BackendService) {
    }

    jobs: Job[] = [];

    updateTable() {
        this.backendService.getJobs().then(jobs => {
            this.jobs = [];
            jobs.forEach(job => this.jobs.push(job));
        });
    }

    ngOnInit(): void {
        this.updateTable();
        setInterval(() => {
            this.updateTable();
        }, 2000);
    }

}
