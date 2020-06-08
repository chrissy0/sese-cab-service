import {Component, OnInit} from '@angular/core';
import {Job} from '../job';
import {BackendService} from '../backend.service';
import {faTrashAlt} from '@fortawesome/free-regular-svg-icons/faTrashAlt';

@Component({
    selector: 'app-jobs-display',
    templateUrl: './jobs-display.component.html',
    styleUrls: ['./jobs-display.component.css']
})
export class JobsDisplayComponent implements OnInit {

    constructor(private backendService: BackendService) {
    }

    jobs: Job[] = [];

    trashIcon = faTrashAlt;

    updateTable() {
        this.backendService.getJobs().then(jobs => {
            this.jobs = [];
            jobs.forEach(job => this.jobs.push(job));
        });
    }

    deleteJob(tableIndex: number) {
        this.backendService.deleteJob(this.jobs[tableIndex].id);
    }

    jobsActive() {
        return this.jobs.length > 0;
    }

    ngOnInit(): void {
        this.updateTable();
        setInterval(() => {
            this.updateTable();
        }, 2000);
    }

}
