import {Injectable} from '@angular/core';
import {map, tap, switchMap,filter,first} from 'rxjs/operators';

import {Player} from '../models/Player.model';
import {Observable, of} from 'rxjs';
import {HttpClient} from '@angular/common/http';
import { AppConfigService } from './app-config.service';

@Injectable({
  providedIn: 'root'
})
export class PlayerService {

  constructor(private config:AppConfigService, private http: HttpClient) { }

  public getPlayers(): Observable<Player[]> {
    var health = this.config.loadAppConfig().pipe(
      tap(x=> console.log(" I am healthchecking")),
      switchMap( conf => this.http.get(conf.api_url+"/api/healthcheck")),
      tap(x => console.log("Found:" + JSON.stringify(x)))
    )

    return this.config.loadAppConfig().pipe(
      tap(con => console.log("Got Config with base_url:" + con.api_url)),
      map(con => con.api_url+"/api/player_summary"),
      tap(url=> console.log("HTTPRequesting:" +url)),
      switchMap(url => this.http.get<Player[]>(url))
    );
  }

  public getPlayerByID(id: number): Observable<Player> {
    return this.getPlayers().pipe(
      map(obs => obs.filter(p=>p.player_id===id)),
      map(obs => obs[0])
    );
  }

  public getPlayerByName(name: string): Observable<Player> {
    return this.getPlayers().pipe(
      map(obs => obs.filter(p=>p.player_name===name)),
      map(obs => obs[0])
    );
  }

}
