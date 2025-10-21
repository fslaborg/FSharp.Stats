```

BenchmarkDotNet v0.14.0, Windows 11 (10.0.26100.3775)
13th Gen Intel Core i7-13800H, 1 CPU, 20 logical and 14 physical cores
.NET SDK 9.0.201
  [Host]     : .NET 8.0.14 (8.0.1425.11118), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 8.0.14 (8.0.1425.11118), X64 RyuJIT AVX2


```
| Method     | Size  | Mean        | Error     | StdDev    | Gen0   | Allocated |
|----------- |------ |------------:|----------:|----------:|-------:|----------:|
| **Add**        | **100**   |    **34.90 ns** |  **0.620 ns** |  **0.580 ns** | **0.0656** |     **824 B** |
| Subtract   | 100   |    34.40 ns |  0.533 ns |  0.445 ns | 0.0656 |     824 B |
| DotProduct | 100   |    17.35 ns |  0.160 ns |  0.150 ns |      - |         - |
| Norm       | 100   |    17.25 ns |  0.085 ns |  0.076 ns |      - |         - |
| **Add**        | **1000**  |   **336.82 ns** |  **3.095 ns** |  **2.585 ns** | **0.6390** |    **8024 B** |
| Subtract   | 1000  |   342.57 ns |  6.743 ns | 11.631 ns | 0.6390 |    8024 B |
| DotProduct | 1000  |   166.93 ns |  2.838 ns |  3.154 ns |      - |         - |
| Norm       | 1000  |   166.51 ns |  0.808 ns |  0.756 ns |      - |         - |
| **Add**        | **10000** | **3,557.53 ns** |  **8.350 ns** |  **7.402 ns** | **6.3286** |   **80024 B** |
| Subtract   | 10000 | 3,563.04 ns | 11.255 ns |  9.977 ns | 6.3286 |   80024 B |
| DotProduct | 10000 | 1,824.85 ns | 27.335 ns | 24.232 ns |      - |         - |
| Norm       | 10000 | 1,639.84 ns | 15.430 ns | 14.433 ns |      - |         - |
