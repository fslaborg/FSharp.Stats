```

BenchmarkDotNet v0.14.0, Windows 11 (10.0.26100.3775)
13th Gen Intel Core i7-13800H, 1 CPU, 20 logical and 14 physical cores
.NET SDK 9.0.201
  [Host]     : .NET 8.0.14 (8.0.1425.11118), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 8.0.14 (8.0.1425.11118), X64 RyuJIT AVX2


```
| Method                     | N    | Mean           | Error        | StdDev       | Gen0   | Allocated |
|--------------------------- |----- |---------------:|-------------:|-------------:|-------:|----------:|
| **SolveTriangularSystemLower** | **10**   |       **186.3 ns** |      **1.23 ns** |      **1.09 ns** | **0.0172** |     **216 B** |
| SolveTriangularSystemUpper | 10   |       185.9 ns |      1.33 ns |      1.18 ns | 0.0172 |     216 B |
| **SolveTriangularSystemLower** | **500**  |   **372,737.0 ns** |  **2,926.50 ns** |  **2,737.45 ns** | **0.4883** |    **8056 B** |
| SolveTriangularSystemUpper | 500  |   357,818.8 ns |  1,729.90 ns |  1,618.15 ns | 0.4883 |    8056 B |
| **SolveTriangularSystemLower** | **1000** | **1,510,031.5 ns** |  **7,131.73 ns** |  **6,671.03 ns** |      **-** |   **16057 B** |
| SolveTriangularSystemUpper | 1000 | 1,422,197.8 ns |  8,642.37 ns |  8,084.08 ns |      - |   16057 B |
| **SolveTriangularSystemLower** | **2000** | **6,119,478.8 ns** | **40,341.69 ns** | **37,735.64 ns** |      **-** |   **32059 B** |
| SolveTriangularSystemUpper | 2000 | 5,776,260.6 ns | 30,122.54 ns | 28,176.64 ns |      - |   32059 B |
