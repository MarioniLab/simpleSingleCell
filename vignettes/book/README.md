# Rmarkdown files for Orchestrating Single Cell Analyses

## Overview

This repository contains the basic ingredients for the [Orchestrating Single Cell Analysis](https://osca.bioconductor.org) book.
Developers wanting to contribute scientific content to the book should make pull requests to this repository, 
the others with `-release` or `-devel` suffixes are just used to host the book content on GitHub Pages.
Our set-up provides a light code-only repository (this one) for day-to-day developer use,
which avoids the Git blob bloat from storing PNGs and HTMLs in the other repository.

## Structure

The book is split into four parts:

1. Introductory chapters focusing on how to install and use R and Bioconductor.
2. Topic chapters, the meat of the book where each chapter describes a different step of a scRNA-seq analysis.
3. Workflows containing end-to-end analysis Rmarkdown reports with minimal explanatory text.
4. Appendices containing some bits and pieces about the contributors.

Compilation of the workflows will cache the objects generated after each chunk.
This allows objects to be quickly re-used in the chapters without having to repeat or rewrite the prior steps.
The `extractCached()` calls littered in the chapters will extract objects of interest from each cache,
also reporting the steps used to generate those objects in a folded code chunk.
This enables readers of each chapter to inspect the code without interrupting the pedagogical flow.

As a consequence, compilation of many of the chapters depends on compilation of the workflows.
Those writing new chapters should move all set-up code into a similar workflow 
and exploit the `extractCached()` to obtain a starting point for their chapter.
Also note the `chapterPreamble()` code chunk that is required at the top of each chapter to set up the collapsible elements.

## Build instructions

Install the **rebook** package from https://github.com/LTLA/rebook.
Then, run the usual **bookdown** invocation, for example:

```r
bookdown::render_book("index.Rmd", "bookdown::gitbook")
```

Advanced users can call `make` to perform a "pre-compilation" prior to the above command.
This generates cached content to be used by the serial **bookdown** invocation,
and is most useful when the `make` itself is parallelized.

```sh
make
rm -rf _bookdown_files
R -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'
```

The docker image contains the book in `/home/book` where these commands can be executed.

## Developer instructions

To contribute reports, follow standard procedure: fork and PR.

- All `topic` chapters must start from a `SingleCellExperiment` object and should be independent of other `topic` chapters.
- All `workflow` chapters should use a `SingleCellExperiment` object throughout the various chunks.
This allows chapters to pick up the SCE at any point.

Any changes should be accompanied by house-keeping updates:

- Updating the listed dependencies in the `DESCRIPTION`.
Some `extra` help is required for those dependencies that are implicitly required via `Suggests`.

  ```r
  rebook::updateDependencies(extra=c("Rtsne", "RMTstat", "statmod", "GO.db"))
  ```

- Updating the `Makefile` for parallel builds.

  ```r
  createMakefile()
  ```
