import { ComponentFixture, TestBed, async } from '@angular/core/testing';

import { AppConfigService } from 'src/app/services/app-config.service';
import { TeamDetailComponent } from './team-detail.component';
import { TeamService } from '../../services/team.service';

describe('TeamDetailComponent', () => {
  let component: TeamDetailComponent;
  let fixture: ComponentFixture<TeamDetailComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [TeamDetailComponent],
      providers: [
        {
          provide: TeamService,
          useValue: jasmine.createSpyObj<TeamService>(['getAll'])
        },
        {
          provide: AppConfigService,
          useValue: jasmine.createSpyObj<AppConfigService>(['loadAppConfig'])
        }
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TeamDetailComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
