import { Component, OnInit } from '@angular/core';

import {SelectedService} from '../../services/selected.service';
import { PlayerService } from 'src/app/services/player.service';
import { Player } from 'src/app/models/Player.model';
import { Observable } from 'rxjs';

@Component({
  selector: 'app-player-splitscreen-page',
  templateUrl: './player-splitscreen-page.component.html',
  styleUrls: ['./player-splitscreen-page.component.css'],
  providers: [SelectedService]
})
export class PlayerSplitscreenPageComponent {
  players: Observable<Player[]> = this.playerService.getAll();

  constructor(private playerService: PlayerService) { }
}
