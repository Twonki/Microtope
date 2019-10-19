import { AppConfigService } from './app-config.service';
import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed } from '@angular/core/testing';

describe('AppConfigService', () => {
  let service: AppConfigService;
  let httpClientSpy: jasmine.SpyObj<HttpClient>;

  beforeEach(() => {
    const httpSpy = jasmine.createSpyObj('HttpClient', ['get', 'put', 'push']);

    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      // Provide both the service-to-test and its (spy) dependency
      providers: [AppConfigService, { provide: HttpClient, useValue: httpSpy }]
    });
    // Inject both the service-to-test and its (spy) dependency
    service = TestBed.get(AppConfigService);
    httpClientSpy = TestBed.get(HttpClient);
  });

  it('should be created', () => {
    const service: AppConfigService = TestBed.get(AppConfigService);
    expect(service).toBeTruthy();
  });
});
