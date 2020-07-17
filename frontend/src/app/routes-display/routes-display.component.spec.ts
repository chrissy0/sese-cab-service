import {async, ComponentFixture, TestBed} from '@angular/core/testing';

import {RoutesDisplayComponent} from './routes-display.component';

describe('RoutesDisplayComponent', () => {
  let component: RoutesDisplayComponent;
  let fixture: ComponentFixture<RoutesDisplayComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [RoutesDisplayComponent]
    })
        .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(RoutesDisplayComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
