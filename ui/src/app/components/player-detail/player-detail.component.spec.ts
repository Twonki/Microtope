import { ComponentFixture, TestBed, async } from '@angular/core/testing';

import { AppConfigService } from '../../services/app-config.service';
import { PlayerDetailComponent } from './player-detail.component';
import { PlayerService } from '../../services/player.service';

describe('PlayerDetailComponent', () => {
  let component: PlayerDetailComponent;
  let fixture: ComponentFixture<PlayerDetailComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [PlayerDetailComponent],

      providers: [
        {
          provide: PlayerService,
          useValue: jasmine.createSpyObj<PlayerService>(['getAll'])
        },
        {
          provide: AppConfigService,
          useValue: jasmine.createSpyObj<AppConfigService>(['loadAppConfig'])
        }
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(PlayerDetailComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
