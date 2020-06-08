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
        this.http.post('http://localhost:8080/bookr/job', job).toPromise().then(response => console.log(response));
    }
}
