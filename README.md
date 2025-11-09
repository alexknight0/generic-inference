# Generic Inference
An experimental Haskell library for the computation of Generic Inference. Detailed in the paper "Implementation of Generic Inference in Haskell".

# Importing as a dependency

The library can be added as a dependency by linking this git repo.

To do this, add the following under `dependencies` in `package.yaml`:

```
dependencies:
- generic-inference
```

and add the following under `extra-deps` in `stack.yaml`:

```
extra-deps:
  - git: https://github.com/alexknight0/generic-inference.git
    commit: 2bdfecc62ac7e355e7611abd3c4f605cb7e50237  # <-- Commit you want the library at

  # Stack will likely tell you to add these as well...
  - dijkstra-simple-0.1.0@sha256:c8ef4401c26369aaea1531eafb59a253cfe9b88f5608c4874e7a9e4d64d0133a,1676
  - distributed-process-0.7.8@sha256:5f33886c431444a874df7c54e30d0a13913eb4f8a39785924b7e6b648ed5efb1,7682
  - network-transport-tcp-0.8.6@sha256:82818f659c4324f1ef7d008803ee362669cbcef66c93bd00367e3370d547a813,3792
  - fingertree-0.1.4.2@sha256:b368fda61b14073bd798f45c8fa91e1c5331628193a8b88d93fb5a4341849c64,2063
```

If you encounter issues, consider using the `resolver` `lts-22.20`. To do this, add the following to `stack.yaml` (removing whatever `snapshot` or `resolver` you were using previously):
```
resolver: lts-22.20
```

## Diagrams
The join tree diagrams produced can be **large**. If you are in need of a SVG viewer that can zoom in further than your browser, consider 'Nomacs'.

Your show instance for a valuation algebra will be displayed in the graph. Make sure to use newlines on long output;
otherwise the diagram will likely render so wide that nothing will be legible.

# Repository Structure

## Building the project
The project uses `stack`, and can hence be built with `stack build`.

To run tests, run `stack test --fast` (the `--fast` prevents GHC optimizing out certain asserts)

To benchmark, run `stack bench --benchmark-arguments "--output report.html" --profile`.
This will output a `report.html` detailing some benchmarking details, as well as a `generic-inference.prof` that will detail what functions were most computationally intensive.

## Commands
(I've tried to reliably add these to the `package.yaml` but it has had mixed results, feel free to add them in if you're smarter than me!)

### Testing

For testing consider:

```
stack test --fast
```
(the --fast is important to prevent the `assert`s from being compiled out)

### Benchmarking

For benchmarking performance I use:

```
stack bench --ghc-options="-O2" --benchmark-arguments "+RTS -s -RTS --csv raw_benchmarks.csv  --output report.html"
```
(append `--profile` for profiling, noting that this significantly slows down the run times!)

For recording the number of operations consider:

```
stack bench --ghc-options="-O2 -fno-full-laziness -fno-cse -DCOUNT_OPERATIONS=1" --benchmark-arguments "ops"
```
(The `-fno-full-laziness` and `-fno-cse` are used to help ensure usage of `unsafePerformIO` is safe (see wiki if confused). `-DCOUNT_OPERATIONS=1` passes a c preprocessor flag that enables counting the number of invocations of certain functions (which may have a small runtime cost when enabled))

## Directory Structure
```
-- Main Library
└── src
    └── GenericInference
        ├── Inference.hs
        ├── Inference
        │   │
        │   │ -- Join Tree / Elimination Sequence Construction
        │   ├── EliminationSequence.hs
        │   ├── Triangulation.hs
        │   ├── JoinTree.hs
        │   ├── JoinTree
        │   │   ├── Diagram.hs
        │   │   ├── Forest.hs
        │   │   └── Tree.hs
        │   │  
        │   │ -- Inference Methods
        │   ├── Fusion.hs
        │   ├── ShenoyShafer.hs
        │   │  
        │   │ -- Solution Construction
        │   ├── DynamicProgramming.hs
        │   │  
        │   ├── MessagePassing.hs
        │   ├── MessagePassing
        │   │   ├── Distributed.hs
        │   │   └── Threads.hs
        │   └── Statistics.hs
        │
        │ -- Data structures
        ├── LabelledMatrix.hs
        ├── Potential.hs
        ├── IndexedSet.hs
        ├── Graph.hs
        ├── Graph
        │   └── Undirected.hs
        │
        │ -- Problems solved using Valuation Algebra Families
        ├── Problems
        │   ├── BayesianNetwork.hs
        │   ├── BayesianNetwork
        │   │   └── Parser.hs
        │   ├── Database.hs
        │   ├── FastFourierTransform.hs
        │   ├── ShortestDistance.hs
        │   └── ShortestDistance
        │       └── Parser.hs
        │
        | -- Utilities
        ├── LocalProcess.hs
        ├── Pretty.hs
        ├── Utils.hs
        ├── Utils
        │   └── Composition.hs
        │
        │ -- Implementations of Valuation Algebra Families
        ├── ValuationAlgebra.hs
        └── ValuationAlgebra
            ├── QuasiRegular.hs
            ├── QuasiRegular
            │   └── Value.hs
            ├── Semiring.hs
            └── Semiring
                └── Value.hs

-- Benchmarks
└── src
    ├── Benchmarks.hs
    └── Benchmarks
        ├── BayesianNetwork.hs
        ├── BayesianNetwork
        │   ├── Baseline.hs
        │   ├── Data.hs
        │   └── Data
        │       ├── alarm.net
        │       ├── andes.net
        │       ├── answers.r
        │       ├── asia.net
        │       ├── child.net
        │       └── munin.net
        ├── FastFourierTransform.hs
        ├── FastFourierTransform
        │   ├── Baseline.hs
        │   └── Data.hs
        ├── ShortestDistance.hs
        ├── ShortestDistance
        │   ├── Baseline.hs
        │   ├── Data.hs
        │   └── Data
        │       ├── Large-USA-road-d.NY.gr
        │       ├── Medium-USA-road-d.NY.gr
        │       ├── Small-USA-road-d.NY.gr
        │       ├── USA-road-d.NY.gr
        │       ├── VerySmall-USA-road-d.NY.gr
        │       └── VeryVeryLarge-USA-road-d.NY.gr
        └── Utils.hs

-- Testing
└── src
    ├── Tests.hs
    └── Tests
        ├── BayesianNetwork.hs
        ├── FastFourierTransform.hs
        ├── LabelledMatrix.hs
        ├── ShortestDistance.hs
        └── Utils.hs

-- Stack configuration
├── stack.yaml
└── package.yaml

-- Stack Boilerplate 
├── Setup.hs
├── app
│   └── Main.hs
├── test
│   └── Spec.hs
├── bench
│   └── Bench.hs
├── generic-inference.cabal
├── stack.yaml.lock
└── hie.yaml
```

