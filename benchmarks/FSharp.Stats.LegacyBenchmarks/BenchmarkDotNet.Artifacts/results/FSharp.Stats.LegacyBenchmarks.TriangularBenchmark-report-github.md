```

BenchmarkDotNet v0.14.0, Windows 11 (10.0.26100.3775)
13th Gen Intel Core i7-13800H, 1 CPU, 20 logical and 14 physical cores
.NET SDK 9.0.201
  [Host]     : .NET 8.0.14 (8.0.1425.11118), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 8.0.14 (8.0.1425.11118), X64 RyuJIT AVX2


```
| Method                     | N    | Mean            | Error        | StdDev        | Gen0     | Gen1     | Gen2     | Allocated   |
|--------------------------- |----- |----------------:|-------------:|--------------:|---------:|---------:|---------:|------------:|
| **SolveTriangularSystemLower** | **10**   |        **731.0 ns** |      **5.15 ns** |       **4.82 ns** |   **0.1097** |        **-** |        **-** |     **1.34 KB** |
| SolveTriangularSystemUpper | 10   |        665.6 ns |      5.93 ns |       5.54 ns |   0.1097 |        - |        - |     1.34 KB |
| **SolveTriangularSystemLower** | **500**  |  **1,474,179.5 ns** | **29,026.42 ns** |  **32,262.78 ns** | **332.0313** | **330.0781** | **330.0781** |  **1977.87 KB** |
| SolveTriangularSystemUpper | 500  |  1,508,533.3 ns | 25,877.28 ns |  24,205.62 ns | 333.9844 | 332.0313 | 332.0313 |  1977.72 KB |
| **SolveTriangularSystemLower** | **1000** |  **6,405,608.8 ns** | **83,698.88 ns** |  **78,291.99 ns** | **390.6250** | **390.6250** | **390.6250** |  **7860.04 KB** |
| SolveTriangularSystemUpper | 1000 |  6,555,258.3 ns | 81,557.32 ns |  76,288.76 ns | 390.6250 | 390.6250 | 390.6250 |  7859.79 KB |
| **SolveTriangularSystemLower** | **2000** | **26,497,907.6 ns** | **81,721.04 ns** | **189,400.94 ns** | **875.0000** | **875.0000** | **875.0000** | **31344.14 KB** |
| SolveTriangularSystemUpper | 2000 | 27,361,796.9 ns | 98,627.77 ns | 196,970.33 ns | 875.0000 | 875.0000 | 875.0000 | 31344.14 KB |
