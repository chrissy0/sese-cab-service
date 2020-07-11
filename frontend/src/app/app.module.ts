import {BrowserModule} from '@angular/platform-browser';
import {NgModule} from '@angular/core';

import {AppRoutingModule} from './app-routing.module';
import {AppComponent} from './app.component';
import {BookingFormComponent} from './booking-form/booking-form.component';
import {FormsModule} from '@angular/forms';
import {HttpClientModule} from '@angular/common/http';
import {JobsDisplayComponent} from './jobs-display/jobs-display.component';
import {FontAwesomeModule} from '@fortawesome/angular-fontawesome';
import {RoutesDisplayComponent} from './routes-display/routes-display.component';
import {DebugComponent} from './debug/debug.component';

@NgModule({
  declarations: [
    AppComponent,
    BookingFormComponent,
    JobsDisplayComponent,
    RoutesDisplayComponent,
    DebugComponent
  ],
  imports: [
    BrowserModule,
    FormsModule,
    AppRoutingModule,
    HttpClientModule,
    FontAwesomeModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
