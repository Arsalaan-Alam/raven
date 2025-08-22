# The Raven Ecosystem

Raven is a collection of libraries designed to work together seamlessly. Understanding their roles and relationships is key to using the ecosystem effectively.

## Understanding the Ecosystem

The Raven ecosystem is organized around three core layers:

- **Foundation Libraries (Nx, Saga)**: The foundation providing n-dimensional arrays and text processing. These reusable cores serve both Raven and the broader OCaml community, with Nx offering NumPy-like functionality with pluggable backends and Saga providing modern text tokenization.
- **Rune for Differentiable Computing**: The middle layer enabling automatic differentiation and GPU acceleration. Built on Nx, it transforms regular computations into differentiable ones, providing the mathematical machinery for gradient-based optimization.
- **Domain Frameworks (Kaun, Sowilo)**: The top layer delivering specialized tools for specific domains. These frameworks leverage Rune's differentiable computing to provide high-level APIs for deep learning and computer vision tasks.

Alongside these core layers, Raven includes **supporting tools** for the development experience: **Hugin** for publication-quality plotting and **Quill** for interactive notebooks, making it easy to visualize results and explore ideas.

```
     ┌─────────────────────────────────────────────┐
     │         Domain-Specific Frameworks          │
     │     ┌──────────┐        ┌───────────┐       │
     │     │   Kaun   │        │   Sowilo  │       │
     │     │   (DL)   │        │  (Vision) │       │
     │     └────┬─────┘        └────┬──────┘       │
     └──────────┼───────────────────┼──────────────┘
                │                   │
     ┌──────────▼───────────────────▼────────────┐
     │        Differentiable Computing           │
     │              ┌────────────┐               │
     │              │    Rune    │               │
     │              │ (Autodiff) │               │
     │              └─────┬──────┘               │
     └──────────────────┼────────────────────────┘
                        │
     ┌──────────────────▼─────────────────────┐
     │       Foundation Libraries             │
     │  ┌────┐  ┌─────────────┐  ┌─────────┐  │
     │  │ Nx │  │ Nx_datasets │  │  Saga   │  │
     │  └────┘  └─────────────┘  └─────────┘  │
     └────────────────────────────────────────┘

         Supporting Tools & Visualization
     ┌──────────────┐         ┌─────────────┐
     │    Hugin     │         │    Quill    │
     │  (Plotting)  │         │ (Notebooks) │
     └──────────────┘         └─────────────┘
```

## Core Libraries

### Nx: The Data Foundation

**What it is**: N-dimensional arrays with pluggable backends, our NumPy equivalent.

**When to use it**: 
- Loading and manipulating numerical data
- Data preprocessing and cleaning
- Non-differentiable numerical computations
- I/O operations (reading/writing NumPy files, images)

**Key features**:
- Broadcasting and indexing like NumPy
- Multiple backends (CPU, Metal GPU)
- Efficient memory views and slicing
- Interop with NumPy through .npy/.npz files

### Saga: Text Processing

**What it is**: Modern text tokenization and processing library, our Tokenizers/spaCy equivalent.

**When to use it**:
- Text tokenization for NLP models
- Building custom vocabularies
- Batch encoding text for neural networks
- Unicode text normalization and cleaning

**Key features**:
- Multiple tokenizers (BPE, WordPiece, word/character-level)
- Unicode-aware text processing
- Efficient vocabulary management
- Direct integration with Nx for tensor encoding
- Batch processing with padding/truncation

### Rune: The Differentiable Core

**What it is**: Automatic differentiation with multi-device support, our JAX equivalent.

**When to use it**:
- Building differentiable models
- Computing gradients for optimization
- GPU acceleration
- JIT compilation (coming soon - see [roadmap](/docs/roadmap/))

**Key features**:
- Effect-based automatic differentiation
- Device placement (CPU/GPU)
- Functional transformations (grad, vmap)
- JIT compilation for performance (coming soon)

**Relationship to Nx**: Rune tensors (`Rune.t`) are distinct from Nx arrays (`Nx.t`). Convert between them:
```ocaml
(* Nx to Rune *)
let rune_tensor = Rune.of_bigarray device (Nx.to_bigarray nx_array)

(* Rune to Nx *)
let nx_array = Nx.of_bigarray (Rune.unsafe_to_bigarray rune_tensor)
```

## Application Frameworks

### Kaun: Deep Learning

**What it is**: Functional neural network framework built on Rune, our Flax equivalent.

**When to use it**:
- Building neural networks
- Training deep learning models
- Using pre-built layers and optimizers
- Managing randomness with PRNGs

**Key features**:
- Standard layers (Linear, Conv2D, BatchNorm)
- Optimizers (SGD, Adam, AdamW)
- Loss functions
- PRNG support for reproducible randomness
- Dataset utilities

### Sowilo: Computer Vision

**What it is**: Differentiable image processing and computer vision built on Rune.

**When to use it**:
- Differentiable image preprocessing for ML
- Building vision models with trainable augmentations
- Classical computer vision algorithms in differentiable pipelines

**Key features**:
- Image transformations (resize, crop, flip) - all differentiable
- Filters (Gaussian blur, edge detection)
- Color space conversions
- Morphological operations
- Compatible with Rune's autodiff for end-to-end training

## Tooling Stack

### Hugin: Visualization

**What it is**: Publication-quality plotting library, our Matplotlib equivalent.

**How it fits**: Takes Nx arrays as input to create visualizations.

**Key features**:
- 2D and 3D plotting
- Multiple plot types (line, scatter, bar, histogram)
- Subplots and figure composition
- Export to PNG/PDF

### Quill: Interactive Notebooks

**What it is**: A writing-first notebook environment that treats notebooks as markdown documents with executable code.

**How it fits**: The unified environment for combining all Raven libraries in exploratory workflows.

**Key features**:
- Markdown files as notebooks - no JSON, no cells
- Native support for all Raven libraries pre-loaded
- Integrated Hugin visualizations render inline
- Version control friendly - diffs like regular markdown
- Writing-focused: code enhances narrative, not vice versa

## Typical Workflows

### Data Analysis Workflow

```ocaml
(* Load data with Nx *)
let data = Nx_io.read_npy "data.npy" in

(* Analyze with Nx operations *)
let mean = Nx.mean ~axis:[|0|] data in
let std = Nx.std ~axis:[|0|] data in

(* Visualize with Hugin *)
let fig = Hugin.figure () in
let ax = Hugin.add_subplot fig ~nrows:1 ~ncols:1 ~index:1 in
Hugin.hist ax (Nx.to_array data) ~bins:30;
Hugin.show fig
```

### Machine Learning Workflow

```ocaml
(* Load dataset with Kaun_datasets *)
let train_dataset = Kaun_datasets.mnist ~batch_size:32 () in

(* Define model with Kaun *)
let model = Kaun.Sequential.create [
  Kaun.Linear.create ~in_features:784 ~out_features:128;
  Kaun.relu;
  Kaun.Linear.create ~in_features:128 ~out_features:10;
] in

(* Training loop *)
let train_step model opt x y =
  let loss, grads = Rune.grad_and_value (fun model ->
    let logits = model x in
    Kaun.cross_entropy_loss logits y
  ) model in
  let model = Kaun.Optimizer.update opt model grads in
  model, loss
```

### Computer Vision Workflow

```ocaml
(* Load image with Nx *)
let img = Nx_io.read_image "photo.jpg" in

(* Convert to Rune and preprocess with Sowilo *)
let device = Rune.Cpu.device in
let img_rune = Rune.of_bigarray device (Nx.to_bigarray img) in

(* Apply transformations *)
let preprocessed = 
  img_rune
  |> Sowilo.resize ~width:224 ~height:224
  |> Sowilo.to_float32  (* Convert to float *)
  |> Rune.div_scalar 255.  (* Normalize to [0,1] *)
  
(* Use in vision model *)
let output = vision_model preprocessed
```

### NLP Workflow

```ocaml
(* Tokenize text with Saga *)
let texts = ["Hello world"; "This is a test"] in

(* Build vocabulary from corpus *)
let vocab = Saga.vocab ~max_size:10000 (
  List.concat_map Saga.tokenize corpus
) in

(* Encode text to tensors *)
let input_ids = Saga.encode_batch ~vocab ~max_len:128 texts in

(* Convert to Rune for model processing *)
let device = Rune.Cpu.device in
let input_tensor = Rune.of_bigarray device (Nx.to_bigarray input_ids) in

(* Process through language model *)
let output = language_model input_tensor in

(* Decode predictions back to text *)
let predicted_ids = (* get predictions from output *) in
let predicted_text = Saga.decode vocab predicted_ids
```

## Getting Started

1. **For numerical computing**: Start with [Nx](nx/getting-started.md)
2. **For text processing**: Use [Saga](saga/getting-started.md)
3. **For plotting**: Use [Hugin](hugin/getting-started.md)
4. **For machine learning**: Move to [Rune](rune/getting-started.md) and [Kaun](kaun/getting-started.md)
5. **For interactive work**: Use [Quill](quill/getting-started.md)

Each library has its own getting-started guide with examples to help you begin.