import { AppConfigService } from './app-config.service';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TeamService } from './team.service';
import { TestBed } from '@angular/core/testing';

describe('TeamService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [
        TeamService,
        {
          provide: AppConfigService,
          useValue: jasmine.createSpyObj<AppConfigService>(['loadAppConfig'])
        }
      ]
    }).compileComponents();
  });

  it('should be created', () => {
    const service: TeamService = TestBed.get(TeamService);
    expect(service).toBeTruthy();
  });
});
