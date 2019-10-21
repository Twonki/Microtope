import { ComponentFixture, TestBed, async } from '@angular/core/testing';

import { TeamService } from '../../services/team.service';
import { TeamSplitscreenPageComponent } from './team-splitscreen-page.component';

describe('TeamSplitscreenPageComponent', () => {
  let component: TeamSplitscreenPageComponent;
  let fixture: ComponentFixture<TeamSplitscreenPageComponent>;

  beforeEach(async(() => {
    const spy = jasmine.createSpyObj('teamTeamService', ['getAll']);
    TestBed.configureTestingModule({
      declarations: [TeamSplitscreenPageComponent],
      providers: [{ provide: TeamService, useValue: spy }]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TeamSplitscreenPageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('has no items - should be created', () => {
    // component.teams = []

    expect(component).toBeTruthy();
  });
});
