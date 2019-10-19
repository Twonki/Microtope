import { ComponentFixture, TestBed, async } from '@angular/core/testing';

import { AppConfigService } from '../../services/app-config.service';
import { PlayerService } from '../../services/player.service';
import { PlayerSplitscreenPageComponent } from './player-splitscreen-page.component';

describe('PlayerSplitscreenPageComponent', () => {
  let component: PlayerSplitscreenPageComponent;
  let fixture: ComponentFixture<PlayerSplitscreenPageComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [PlayerSplitscreenPageComponent],
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
    fixture = TestBed.createComponent(PlayerSplitscreenPageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
