import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { JobsDisplayComponent } from './jobs-display.component';

describe('JobsDisplayComponent', () => {
  let component: JobsDisplayComponent;
  let fixture: ComponentFixture<JobsDisplayComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ JobsDisplayComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(JobsDisplayComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
