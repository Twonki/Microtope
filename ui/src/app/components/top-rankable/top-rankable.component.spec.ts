import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SelectedService } from '../../services/selected.service';
import { TopRankableComponent } from './top-rankable.component';
import { of } from 'rxjs';

describe(`${TopRankableComponent.constructor.name}`, () => {
  let fixture: ComponentFixture<TopRankableComponent<any>>;
  let component: TopRankableComponent<any>;

  beforeEach(() => {
    const spy = jasmine.createSpyObj('select', ['selected$']);
    TestBed.configureTestingModule({
      declarations: [TopRankableComponent],
      providers: [{ provide: SelectedService, useValue: spy }]
    }).compileComponents();

    fixture = TestBed.createComponent(TopRankableComponent);
    component = fixture.componentInstance;
  });

  it('should return empty Array if given empty Array', done => {
    // Simulate "Input"
    component.items = of([]);

    // Trigger change detection, this is where ngOninit runs
    fixture.detectChanges();

    component.sortedItems.subscribe(items => {
      expect(items).toEqual([]);
      done();
    });
  });
});
