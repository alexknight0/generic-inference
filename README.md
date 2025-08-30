# Local Computation
## Directory Structure

```

.
├── app
│   └── Main.hs  // The entry point for the main executable.
|                // Not currently used for anything useful.
├── bench
│   └── Bench.hs // The entry point for benchmarking.
|                // Just calls function in src/Benchmark.hs
└── test
|   └── Spec.hs  // The entry point for tests.
|                // Just calls function in src/Tests.hs
|
├── src    // The main directory. All source files are contained here,
|   |      // including the main library code, as well as code for
|   |      // benchmarking and testing. This is done as HLS support for
|   |      // multi-cradle stack set-ups is somewhat lacking.
|   |
│   ├── Benchmark                           // Code for benchmarking
│   │   ├── Baseline
│   │   │   ├── DjikstraSimple.hs
│   │   │   ├── FFT.hs
│   │   │   └── Probability.hs
│   │   ├── Data
│   │   │   ├── BayesianNetwork
│   │   │   │   ├── andes.net
│   │   │   │   └── asia.net
│   │   │   └── ShortestPath
│   │   │       ├── Large-USA-road-d.NY.gr
│   │   │       └── Small-USA-road-d.NY.gr
│   │   └── ShortestPath
│   │       └── SingleTarget.hs
│   ├── Benchmark.hs
|   |
|   |
│   ├── LocalComputation                    // Main library code
│   │   ├── Graph.hs
│   │   ├── Inference                       // Code related to implementing inference
│   │   │   ├── Collect.hs
│   │   │   ├── EliminationSequence.hs
│   │   │   ├── JoinTree.hs
│   │   │   └── ShenoyShafer.hs
│   │   ├── Instances                       // Valuation algebra instances applied to
|   |   |   |                               // solve problems
│   │   │   ├── BayesianNetwork
│   │   │   │   └── Parser.hs
│   │   │   ├── BayesianNetwork.hs
│   │   │   ├── FastFourierTransform.hs
│   │   │   └── ShortestPath
│   │   │       ├── Parser.hs
│   │   │       └── SingleTarget.hs
│   │   ├── LabelledMatrix.hs
│   │   ├── LocalProcess.hs
│   │   ├── Utils.hs
│   │   ├── ValuationAlgebra                // Generic valuation algebra structures
│   │   │   ├── QuasiRegular
│   │   │   │   └── SemiringValue.hs
│   │   │   ├── QuasiRegular.hs
│   │   │   ├── Semiring.hs
│   │   │   └── SemiringValue.hs
│   │   └── ValuationAlgebra.hs
|   |
|   | 
│   ├── Tests                              // Tests
│   │   ├── BayesianNetwork
│   │   │   └── Data.hs
│   │   ├── BayesianNetwork.hs
│   │   ├── FastFourierTransform
│   │   │   └── Data.hs
│   │   ├── FastFourierTransform.hs
│   │   ├── LabelledMatrix.hs
│   │   ├── ShortestPath
│   │   │   ├── SingleTarget
│   │   │   │   └── Data.hs
│   │   │   └── SingleTarget.hs
│   │   └── Utils.hs
│   └── Tests.hs
...
```

## Building the project
The project uses `stack`, and can hence be built with `stack build`.

To run tests, run `stack test --fast` (the `--fast` prevents GHC optimizing out certain asserts)

To benchmark, run `stack bench --benchmark-arguments "--output report.html" --profile`.
This will output a `report.html` detailing some benchmarking details, as well as a `localcomputation.prof` that will detail what functions were most computationally intensive.

## Missing Data Files
If benchmarks or tests fail due to missing data files, please contact me and remind me to post links to where they can be found.

