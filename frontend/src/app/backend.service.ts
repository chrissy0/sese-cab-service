import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {Job} from './job';

@Injectable({
    providedIn: 'root'
})
export class BackendService {

    constructor(private http: HttpClient) {
    }

    saveJob(job: Job) {
        return this.http.post('http://localhost:8080/bookr/job', job).toPromise();
    }

    getJobs() {
        return this.http.get('http://localhost:8080/bookr/jobs').toPromise() as Promise<Job[]>;
    }
}
