import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {Job} from './job';

@Injectable({
  providedIn: 'root'
})
export class BackendService {

  constructor(private http: HttpClient) {
  }

  developmentMode = true;

  host = this.developmentMode ? 'localhost' : '167.71.35.10';
  port = 8081;

  saveJob(job: Job) {
    return this.http.post('http://' + this.host + ':' + this.port + '/api/bookr/job', job).toPromise();
  }

  deleteJob(id: number) {
    return this.http.delete('http://' + this.host + ':' + this.port + '/api/bookr/job', {
      params: {
        id: id.toString()
      }
    }).toPromise();
  }

  getJobs() {
    return this.http.get('http://' + this.host + ':' + this.port + '/api/bookr/jobs').toPromise() as Promise<Job[]>;
  }
}
