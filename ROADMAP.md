# Roadmap

This roadmap may change over time. The items below list the main work planned for the next major release and later releases.

Please open an issue in this repository if you have a question, a concern, or a suggestion.

## Table of Contents

- [Nelson 3.0.0](#nelson-300)
  - [Core Language](#core-language)
  - [Performance](#performance)
  - [User Interface](#user-interface)
  - [Graphics and Plotting](#graphics-and-plotting)
  - [NFlow Simulation Engine](#nflow-simulation-engine)
  - [GPU Computing](#gpu-computing)
  - [AI and Machine Learning](#ai-and-machine-learning)
  - [Toolboxes and Extensions](#toolboxes-and-extensions)
  - [Developer Workflow and CI/CD](#developer-workflow-and-cicd)
  - [Packaging and Distribution](#packaging-and-distribution)
- [How Can You Contribute?](#how-can-you-contribute)
- [Contributing to the Roadmap](#contributing-to-the-roadmap)
- [Previous Roadmap](#previous-roadmap)

## Nelson 3.0.0

### Core Language

- Fix bugs and continue regular maintenance
- Refactor the code to make it easier to maintain
- Add more compatible functions
- Add namespace aliases to make migration easier
- Add a Language Server Protocol (LSP) server for IDEs such as VS Code, JetBrains, and Neovim

### Performance

- Improve execution speed
- Use NPU/GPU arrays to speed up code execution
- Add advanced sparse solvers and preconditioners
- Compile frequently used functions ahead of time (AOT) into native code

### User Interface

- Replace the Qt desktop with the full webview-based interface
- Support real-time collaborative editing of notebooks and NFlow models in the web interface

### Graphics and Plotting

- Add a full LaTeX interpreter for figure text
- Continue improving graphics compatibility
- Build interactive web dashboards from Nelson figures

### NFlow Simulation Engine

#### Interoperability

- Import and support external simulation models

#### Modeling Capabilities

- Support acausal modeling across subsystem boundaries
- Extend the block library and improve compatibility
- Link requirements to model blocks, test cases, and generated code

#### Simulation and Solvers

- Improve code generation
- Add differentiable simulation through the NFlow solver loop.
- Add Software-in-the-Loop (SIL) test frameworks for generated code

### GPU Computing

- Extend `gpu_engine` (WebGPU/Dawn) with sparse GPU arrays, level-2 kernel
  fusion through lazy evaluation, and `pagefun` on the GPU
- Improve performance for discrete GPU workloads
- Use the GPU to accelerate Monte Carlo simulations and parameter sweeps for NFlow models

### AI and Machine Learning

- Import and run ONNX models.
- Add Physics-Informed Neural Networks (PINNs) as NFlow blocks.
- Add a simulation assistant that can read JSON-based NFlow models, explain model behavior, suggest parameter values, generate test vectors, and help diagnose simulation failures
- Expand statistics and machine learning features

### Toolboxes and Extensions

- Add advanced signal processing features, including wavelets, adaptive filters, and broader multirate design workflows
- Provide a VS Code extension with LSP support
- Add a Jupyter kernel for running Nelson in Jupyter notebooks

### Developer Workflow and CI/CD

- Run Nelson and NFlow in Docker containers and GitHub Actions without
  license servers
- Make environments reproducible with lockfile-based dependency management
  for packages and toolboxes
- Run NFlow simulations and regression tests from the command line (DONE)
- Store cryptographically signed records of the model version, parameters,
  solver configuration, and results for regulated industries such as those
  covered by DO-178C and ISO 26262

### Packaging and Distribution

- Continue improving packages for Windows, macOS, and Linux

## How Can You Contribute?

The goals above show where the project needs help. If you want to contribute,
comment on a related issue first so that work is not duplicated. If a
maintainer is already assigned to the issue, contact them on GitHub and offer your help.

## Contributing to the Roadmap

This roadmap is open to community changes. You can propose new items or
changes to help the project focus on the most useful work.

## Previous Roadmap

See [Roadmap v2.0.0](ROADMAP-2.0.0.md).
