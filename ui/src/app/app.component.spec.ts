import { TestBed, async } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { AppComponent } from './app.component';
import { HttpClientModule } from '@angular/common/http';
import { BrowserModule } from '@angular/platform-browser';
import { AppRoutingModule } from './app-routing.module';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { PlayerSplitscreenPageComponent } from './components/player-splitscreen-page/player-splitscreen-page.component';
import { TeamSplitscreenPageComponent } from './components/team-splitscreen-page/team-splitscreen-page.component';

describe('AppComponent', () => {
  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientModule,
        BrowserModule,
        AppRoutingModule
        ],
      declarations: [
        AppComponent,
        PlayerSplitscreenPageComponent,
        TeamSplitscreenPageComponent
 // <app-player-splitscreen-page></app-player-splitscreen-page>
 // <app-team-splitscreen-page></app-team-splitscreen-page>
      ],
 //     schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    }).compileComponents();
  }));

  it('should create the app', () => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.debugElement.componentInstance;
    expect(app).toBeTruthy();
  });

});
