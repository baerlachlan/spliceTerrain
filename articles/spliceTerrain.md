# Creating Sashimi-Style Plots with spliceTerrain

## Setup

`BiocManager` is recommended for installing the packages required in
this vignette. `BiocManager` handles installation from Bioconductor,
CRAN and GitHub repositories.

``` r

if (!"BiocManager" %in% rownames(installed.packages()))
    install.packages("BiocManager")
pkgs <- c(
    "spliceTerrain", "RNAseqData.HNRNPC.bam.chr14", "EnsDb.Hsapiens.v75",
    "AnnotationFilter", "ensembldb", "GenomicRanges", "scales", "pander",
    "GenomeInfoDb"
)
BiocManager::install(pkgs, update = FALSE)
```

Now we can load the packages used below.

``` r

library(spliceTerrain)
library(RNAseqData.HNRNPC.bam.chr14)
library(EnsDb.Hsapiens.v75)
library(AnnotationFilter)
library(ensembldb)
library(GenomicRanges)
library(scales)
library(pander)
library(GenomeInfoDb)
```

## Quick start

At a minimum,
[`spliceTerrain()`](https://baerlachlan.github.io/spliceTerrain/reference/spliceTerrain-methods.md)
needs BAM file paths and a genomic interval. Here we use BAM files from
`RNAseqData.HNRNPC.bam.chr14`, selecting one HNRNPC knockdown sample and
one control sample. The region can be supplied as a single character
string.

``` r

bams <- RNAseqData.HNRNPC.bam.chr14_BAMFILES[c(7, 1)]
region <- "chr14:70,233,810-70,238,690"

spliceTerrain(bam = bams, region = region)
```

![](spliceTerrain_files/figure-html/quickstart-1.png)

## Introduction

Sashimi plots visualise RNA-seq alignments across a genomic interval.
They combine coverage tracks with arcs representing splice junction
support. `spliceTerrain` reports both coverage and junction support in
fragment units. This makes them useful for inspecting transcript
structure, exon usage, alternative splice sites, intron retention, and
differences in junction usage between samples.

### Motivation

`spliceTerrain` was designed to fit into R/Bioconductor workflows.
Candidate regions, exon models, and features of interest are often
already represented as `GRanges` or `GRangesList` objects.
`spliceTerrain` allows reuse of these objects for regions, transcript
annotation, and highlights alongside indexed BAM files, within the same
R session. For example, exon models retrieved from an `EnsDb` can become
annotation rows without first exporting them to an annotation file.

`spliceTerrain` brings these inputs together in a focused,
sample-resolved sashimi workflow. It returns a `patchwork` plot or an
inspectable genomic context. Its genomic summaries and plotting settings
can be edited and replotted.

### Related tools

[Gviz](https://bioconductor.org/packages/Gviz/) (Hahne and Ivanek 2016),
[ggbio](https://bioconductor.org/packages/ggbio/) (Yin et al. 2012), and
[ggsashimi](https://github.com/guigolab/ggsashimi) (Garrido-Martín et
al. 2018) provide established approaches to related visualization tasks.

| Tool | Documented focus | Interface |
|:---|:---|:---|
| Gviz | Genomic tracks, coverage and sashimi | R/grid track objects |
| ggbio | Genomic graphics and splice summaries | R/ggplot2-based graphics |
| ggsashimi | Sashimi, overlays and sample aggregation | Python/R command line |

Gviz supports BAM-backed alignment tracks, annotation tracks, junction
filters, and user-supplied junction summaries. ggbio supports
Bioconductor objects, editable graphics, track composition, and gap
truncation. Its `plotSpliceSum()` function summarises support against
transcript models, with optional weighting of reads matching multiple
models. ggsashimi supports optional annotation, junction-derived
shrinking, sample overlays, and mean/median aggregation, and can export
junction BED files or generate an R script for subsequent use. R
integration, editable graphics, and gap compression are therefore shared
capabilities; spliceTerrain’s emphasis is their combination with the
following specific choices:

- **An inspectable genomic context.** `return_ctx = TRUE` exposes
  genomic `GRanges` summaries and settings, and `ctx` rebuilds the
  figure from them. This supports iterative filtering and annotation
  changes with range operations, followed by remapping and plotting
  without another BAM import. Gviz also caches regional data and ggbio
  provides reusable plot objects; spliceTerrain exposes its processed
  genomic summaries as a flat context.
- **A shared, feature-preserving map.** Compression uses retained
  coverage from all samples, supplied annotation, and junction/selection
  anchors to define blocks whose widths are preserved; gaps receive a
  chosen plot-space width. A retained covered segment inside an
  annotated intron can remain visible at its original width, even
  without an annotated exon there. This differs from ggsashimi’s
  junction-derived, length-dependent shrinking. The coverage thresholds
  determine which signal contributes to the map.
- **Explicit counting and usage semantics.** Coverage and junction
  support use fragments, including mapped singletons; overlapping mates
  count once per covered base and once per distinct junction. In the
  reviewed default Gviz alignment-track and ggsashimi counting paths,
  both mates can contribute read-level support. Local junction usage
  uses an unfiltered reference, so hiding an arc does not change another
  arc’s percentage.
- **Annotation as an overlay on observed signal.** Transcript models are
  supplied as a `GRangesList`; metadata controls feature colours and
  labels. `annotated_junctions` distinguishes matching and unmatched
  junctions relative to those models, while retaining the underlying
  fragment counts. This is complementary to ggbio’s model-based
  `plotSpliceSum()` workflow. Arc placement, height, width scaling, and
  panel proportions are explicit plotting controls.
- **Sample-specific controls in one call.** Library sizes and
  normalisation factors, per-BAM strand modes and thresholds, and a
  common y-axis option support comparisons while the samples share one
  coordinate map.

The examples below demonstrate this combination with [transcript
annotation](#adding-transcript-annotation), [compressed
layouts](#compressed-genomic-layouts), [local junction
usage](#local-junction-usage-labels), and [context
reuse](#returning-and-reusing-plot-data). The intended task is visual
inspection of candidate splicing differences at a locus across selected
samples. Gviz and ggbio offer broader genomic visualization frameworks,
while ggsashimi provides a file-driven command-line workflow and sample
aggregation. spliceTerrain uses one panel per BAM and does not perform
differential-splicing tests, event-level PSI estimation, or
transcript-isoform inference.

This comparison was checked on 13 October 2026 against the documented
workflows and source of Gviz 1.57.4 and ggbio 1.61.0 (Bioconductor 3.24
devel), and ggsashimi 1.1.5. The reviewed source snapshots are available
for [Gviz](https://github.com/ivanek/Gviz/tree/482799c06b17),
[ggbio](https://github.com/bioconductor-source/ggbio/tree/5d6ed36bdeb6),
and [ggsashimi](https://github.com/guigolab/ggsashimi/tree/v1.1.5). The
comparison concerns those workflows and defaults, rather than what could
be achieved by supplying custom importers, summaries, or plotting code.

## Complete guide

The following sections build up aspects of a more complete plot and
introduce the main options available in
[`spliceTerrain()`](https://baerlachlan.github.io/spliceTerrain/reference/spliceTerrain-methods.md).
The examples continue to use the HNRNPC chr14 BAM files from the quick
start.

### Input BAM files and regions

The `bam` argument is a character vector of indexed BAM file paths. If
the vector is named, those names are used as sample labels in the plot.
If it is unnamed, labels are derived from the BAM file names. BAM files
must be indexed for region-restricted import. Index files should be
discoverable by `Rsamtools` using standard same-directory naming,
typically `file.bam.bai`.

``` r

bams <- RNAseqData.HNRNPC.bam.chr14_BAMFILES[c(7, 1)]
names(bams) <- c("HNRNPC knockdown", "Control")
```

The `region` argument can be a character string, a `GRanges`, or a
`GRangesList`. Character regions may use comma-separated coordinates,
such as `"chr14:70,233,810-70,238,690"`. For Bioconductor workflows,
using a `GRanges` object is often more convenient.

Because the example BAM files use UCSC style chromosome names, we also
convert the region to UCSC style before plotting.

``` r

ensDb <- EnsDb.Hsapiens.v75
region <- transcripts(ensDb, filter = TxIdFilter("ENST00000394366"))
seqlevelsStyle(region) <- "UCSC"
region
#> GRanges object with 1 range and 6 metadata columns:
#>                   seqnames            ranges strand |           tx_id     tx_biotype tx_cds_seq_start tx_cds_seq_end         gene_id         tx_name
#>                      <Rle>         <IRanges>  <Rle> |     <character>    <character>        <integer>      <integer>     <character>     <character>
#>   ENST00000394366    chr14 70233810-70238690      + | ENST00000394366 protein_coding         70234874       70238178 ENSG00000100650 ENST00000394366
#>   -------
#>   seqinfo: 1 sequence from hg19 genome
```

If multiple ranges are supplied,
[`spliceTerrain()`](https://baerlachlan.github.io/spliceTerrain/reference/spliceTerrain-methods.md)
reduces them to a single span on one chromosome and uses that span as
the BAM query and plotting window. The span is the smallest continuous
interval containing all supplied ranges: it starts at the lowest start
coordinate and ends at the highest end coordinate.

### Adding transcript annotation

Transcript models can be displayed with the `annotation` argument. This
must be a `GRangesList`, where each list element represents one
transcript or feature group. The names of the list elements are used as
y-axis labels in the annotation panel. When annotation is present, the
final plot contains one panel per BAM file plus one annotation panel.
The relative heights of these panels are controlled with
`panel_heights`, where the final value corresponds to the annotation
panel. Setting `annotated_junctions = TRUE` distinguishes junctions that
exactly match an intron in the supplied annotation with solid arcs,
while unmatched junctions are drawn with dashed arcs. Junction
classification is relative to the supplied annotation, so known
junctions may appear unmatched when only a subset of transcripts is
provided.

For this vignette, the plotting region covers the *SRSF5* gene. We
retrieve exon ranges for the gene and keep a small number of transcripts
so the annotation panel remains readable.

``` r

annotation <- exonsBy(
    ensDb, by = "tx", filter = GeneIdFilter("ENSG00000100650")
)
annotation <- annotation[1:3]
seqlevelsStyle(annotation) <- "UCSC"
```

``` r

spliceTerrain(
    bam = bams,
    region = region,
    annotation = annotation,
    annotated_junctions = TRUE,
    panel_heights = c(2, 2, 1)
)
```

![](spliceTerrain_files/figure-html/annotation_plot-1.png)

### Styling annotation tracks

Annotation features can be coloured by a metadata column with
`anno_fill_by`. Use `"group"` to colour each transcript or feature group
separately. Specific fill colours can be supplied with
`anno_fill_colours`.

Text labels can be drawn inside annotation features with
`anno_label_by`. The label colour and size are controlled by
`anno_label_colour` and `anno_label_size`. The `min_arrow` argument
controls how wide an annotation intron must be before directional arrows
are drawn.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    annotation = annotation,
    panel_heights = c(2, 2, 1),
    anno_fill_by = "group",
    anno_fill_colours = c(
        ENST00000553369 = "#66C2A5",
        ENST00000394366 = "#8DA0CB",
        ENST00000451983 = "#FC8D62"
    ),
    anno_label_by = "exon_rank",
    anno_label_colour = "black",
    anno_label_size = 2.5,
    min_arrow = 100
)
```

![](spliceTerrain_files/figure-html/annotation_styling-1.png)

### Highlighting regions of interest

The `highlight` argument marks one or more genomic intervals with shaded
vertical bands. This is useful for drawing attention to a splice site,
variant position, alternative exon, or other feature of interest. The
highlighted interval can be supplied as a character string or as a
`GRanges` object.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    highlight = "chr14:70234854-70234854",
    highlight_colour = "red"
)
```

![](spliceTerrain_files/figure-html/highlight_site-1.png)

A wider interval can be highlighted with a semi-transparent colour.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    highlight = "chr14:70234056-70234097",
    highlight_colour = scales::alpha("red", 0.2)
)
```

![](spliceTerrain_files/figure-html/highlight_exon-1.png)

### Local junction-usage labels

The `junction_usage` argument selects junctions by a genomic position or
range and labels their relative fragment support within each sample.
Junctions are selected when either their start or end anchor overlaps
the supplied interval(s). Junction anchors use intronic coordinates: the
start anchor is the first base of the intron, and the end anchor is the
last base of the intron, not the adjacent exonic bases.

**Selecting a single shared splice site gives the cleanest
interpretation.** For junctions sharing a donor or acceptor, the labels
describe their relative support at that site. By default, the junction
count and usage percentage are separated by a new line. Use
`junction_usage_label_sep = " "` to display them together on one line.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    junction_usage = "chr14:70234854-70234854",
    junction_usage_label_sep = " "
)
```

![](spliceTerrain_files/figure-html/junction_usage_site-1.png)

A range can also be supplied, including one covering multiple splice
sites. It defines a combined set of selected junctions; each junction
enters the denominator once, even if both anchors or overlapping
intervals select it.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    junction_usage = "chr14:70234098-70234854"
)
```

![](spliceTerrain_files/figure-html/junction_usage_range-1.png)

For each sample, the percentage is the junction’s raw fragment support
divided by the summed support of all selected, eligible imported
junctions. This is calculated after import, MAPQ, and strand filtering,
but before `min_junction_reads` filtering or plotting-window exclusion.
An alternative junction remains in the denominator when its arc is
hidden. For example, supports of 90 and 9 give 90.9% and 9.1%; hiding
the second arc with a threshold of 10 leaves the first label at 90.9%,
not 100%. Visible percentages therefore need not sum to 100%.
Library-size normalisation and intron compression do not change the
fractions. Labels are rounded to one decimal place, and no percentage is
shown when the selected support is zero. The reference is conditional on
the imported data: changing the query region, MAPQ threshold, or strand
selection can change the available evidence.

#### Why this is not event-level PSI

Consider a cassette exon between an upstream and a downstream exon. In a
simplified example, 50 inclusion fragments support both inclusion
junctions, and 50 skipping fragments support the direct skipping
junction. A range selecting all three junctions gives the following
junction-usage labels:

``` r

cassette <- data.frame(
    Junction = c("Upstream inclusion", "Downstream inclusion", "Skipping"),
    Fragments = c(50L, 50L, 50L)
)
cassette[["Junction usage (%)"]] <- round(
    100 * cassette$Fragments / sum(cassette$Fragments), 1
)
pander(cassette)
```

|       Junction       | Fragments | Junction usage (%) |
|:--------------------:|:---------:|:------------------:|
|  Upstream inclusion  |    50     |        33.3        |
| Downstream inclusion |    50     |        33.3        |
|       Skipping       |    50     |        33.3        |

The denominator is 150 units of junction support, so each label is
33.3%. There are only 100 fragments in this constructed example, with a
known exon inclusion fraction of 50%: each inclusion fragment supports
two junctions. Thus the 33.3% labels are not estimates of the cassette
exon’s inclusion. Selecting one shared donor or acceptor instead
compares one inclusion junction with the skipping junction and gives 50%
for each in this example. Ranges remain useful for inspecting broader
patterns, but their combined junction fractions should not be
interpreted as general event-level percent spliced in (PSI) or
transcript-isoform proportions.

### Filtering signal

Three arguments control how much signal is imported and displayed.
`min_mapq` filters alignments by mapping quality during BAM import.
`min_coverage` filters positions by per-base fragment coverage before
plotting. Despite its name, `min_junction_reads` filters splice junction
arcs by their supporting fragment count. Both mates supporting the same
junction contribute one count. The latter two arguments also accept
percentage strings such as `"10%"`. Percentage thresholds are calculated
separately for each BAM relative to its maximum raw coverage or junction
count within the plotting region. When supplying one threshold per BAM,
whole-number counts and percentages can be mixed in a character vector.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    min_mapq = 10,
    min_coverage = 10,
    min_junction_reads = "10%"
)
```

![](spliceTerrain_files/figure-html/filtering-1.png)

Technical note: `min_coverage` and `min_junction_reads` are applied to
raw counts before any library-size normalisation is applied.

### Stranded RNA-seq data

The `strandedness` argument describes how read strand should be
interpreted. It can be one of `"unstranded"`, `"forward"`, or
`"reverse"`, and can be supplied either as a single value for all BAM
files or one value per BAM file. The example BAM files are unstranded,
so both read orientations contain signal from the transcript.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    strandedness = "unstranded"
)
```

![](spliceTerrain_files/figure-html/strandedness-1.png)

Specifying a stranded protocol for unstranded BAM files filters reads by
strand. As a result, either `"reverse"` or `"forward"` shows only part
of the coverage that is visible when `strandedness = "unstranded"`.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    strandedness = "forward"
)
```

![](spliceTerrain_files/figure-html/forward_strandedness-1.png)

``` r

spliceTerrain(
    bam = bams,
    region = region,
    strandedness = "reverse"
)
```

![](spliceTerrain_files/figure-html/reverse_strandedness-1.png)

For paired-end BAM files,
[`spliceTerrain()`](https://baerlachlan.github.io/spliceTerrain/reference/spliceTerrain-methods.md)
detects paired-end status from the BAM alignment flags and imports
alignments accordingly. Mapped reads are retained even when their mates
are unmapped. Read-one/read-two flags determine strand interpretation
for both complete pairs and singletons. Unmapped, secondary,
supplementary, and below-`min_mapq` alignments are excluded. If `region`
specifies a strand and `strandedness` is not `"unstranded"`, alignments
are restricted to the resolved region strand.

Coverage and junction support are fragment-level summaries. Each
imported mate group, mapped singleton, or single-end read is one
fragment. Coverage uses the union of each fragment’s aligned blocks.
Overlapping mates contribute at most one count per covered position.
Introns and unsequenced gaps between mates remain uncovered. Each
fragment supports a distinct junction at most once, even if both mates
cross it; the same fragment can support multiple different junctions.
Independent fragments with identical coordinates still count separately.
This is not PCR/UMI deduplication. Keeping mapped singletons avoids
discarding their signal when mate-mapping success differs between
samples.

### Normalising between samples

When samples have different sequencing depths, coverage and junction
counts can be normalised with `lib_size`. Supply full-library sizes in
fragment units before restriction to the plotting region: one per pair
or mapped singleton, or one per read for single-end data. Additional
normalisation factors, such as `edgeR` TMM factors, can be supplied with
`norm_factors`. The effective library size is calculated as
`lib_size * norm_factors`.

By default, counts are normalised to the median effective library size.
Use `normalise_to` to set the target explicitly. The `common_y` argument
is useful when comparing samples because it puts sample panels on the
same y-axis scale.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    lib_size = c(40e6, 55e6),
    norm_factors = c(0.95, 1.08),
    normalise_to = 50e6,
    common_y = TRUE
)
```

![](spliceTerrain_files/figure-html/normalisation-1.png)

### Compressed genomic layouts

By default, `compress_introns = TRUE` compacts large uninformative gaps
so the plot focuses on observed signal. The `intron_width` argument
controls the approximate width of compacted introns in plot space. The
map is built jointly from retained coverage across samples, annotations,
and junction/selection anchors. Retained blocks keep their widths; gaps
between them receive the chosen width. Consequently, retained intronic
coverage or an unannotated covered exon remains part of the displayed
signal rather than being compressed simply because it lies inside an
annotated intron. Changing coverage filters can change the retained
blocks and their map.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    compress_introns = TRUE,
    intron_width = 500
)
```

![](spliceTerrain_files/figure-html/compress_introns-1.png)

If exact genomic distances are more important than compact display,
intron compression can be disabled.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    compress_introns = FALSE
)
```

![](spliceTerrain_files/figure-html/no_compression-1.png)

### Junction arc display

Junction arcs can be tuned with `arc_height`, `arc_side`, and
`arc_scale`. Increasing `arc_height` gives stacked arcs more vertical
separation. The `arc_side` argument controls whether arcs are drawn
above, below, or on both sides of the coverage track. With
`arc_scale = TRUE`, arc width scales with filtered fragment support.
Junction label size is controlled with `junc_text_size`.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    junction_usage = "chr14:70234854-70234854",
    arc_height = 2,
    arc_side = "above",
    arc_scale = TRUE,
    junc_text_size = 2.5
)
```

![](spliceTerrain_files/figure-html/arcs-1.png)

### Sample colours and panel layout

The `colours` argument controls the colours used for sample coverage and
junction arcs. It can be a single colour used for all samples or one
colour per BAM file.

Axis text sizes can be adjusted with `axis_title_size` and
`axis_text_size`.

``` r

spliceTerrain(
    bam = bams,
    region = region,
    annotation = annotation,
    colours = c("darkblue", "darkred"),
    panel_heights = c(2, 2, 1),
    axis_title_size = 20,
    axis_text_size = 12
)
```

![](spliceTerrain_files/figure-html/layout-1.png)

### Returning and reusing plot data

Most users can work directly with the final plot returned by
[`spliceTerrain()`](https://baerlachlan.github.io/spliceTerrain/reference/spliceTerrain-methods.md).
For advanced workflows, set `return_ctx = TRUE` to return the processed
context instead. This contains validated settings, imported alignments,
summarised coverage, junction counts, annotation, and overlays. The
`gal` field contains one alignment object per sample: `GAlignments` for
single-end BAMs or `GAlignmentsList` for paired-end BAMs. The paired-end
list groups imported mates and includes mapped singleton reads. The
`cov` and `juncs` fields contain fragment summaries: `coverage_raw`
holds unnormalised fragment counts and `coverage` holds the plotting
values after any library-size normalisation. The `juncs_all` field
retains eligible junction summaries on the plotting chromosome before
display filtering or clipping, including out-of-window junctions from
imported alignments. It provides a stable reference for the
`usage_fraction` column in `juncs`. That column is on the 0–1 scale,
with `NA` for unselected junctions or an undefined percentage. All
coordinate-bearing fields use genomic coordinates; compressed plot
coordinates are generated internally only when the context is plotted.

``` r

ctx <- spliceTerrain(
    bam = bams,
    region = region,
    annotation = annotation,
    min_junction_reads = 1,
    panel_heights = c(2, 2, 1),
    min_arrow = 100,
    return_ctx = TRUE
)

names(ctx)
#>  [1] "bam"                      "region"                   "annotation"               "junction_usage"           "junction_usage_label_sep" "highlight"                "strandedness"             "min_mapq"                 "min_coverage"             "min_junction_reads"       "lib_size"                 "norm_factors"             "normalise_to"             "compress_introns"         "intron_width"             "min_arrow"                "common_y"                 "arc_height"               "arc_side"                 "arc_scale"                "colours"                  "highlight_colour"         "anno_fill_by"             "anno_fill_colours"        "anno_label_by"            "anno_label_colour"        "anno_label_size"          "junc_text_size"           "panel_heights"            "axis_title_size"          "axis_text_size"           "annotated_junctions"      "gal"                      "cov"                      "juncs"                    "juncs_all"
```

The context can be passed back with the `ctx` argument to redraw the
plot without re-importing BAM files or recalculating coverage and
junctions. For example, the annotation can be used to retain only
coverage overlapping annotated exons and junctions that exactly match
annotated introns.

``` r

ctx$cov <- ctx$cov[overlapsAny(ctx$cov, ctx$annotation)]
ctx$juncs <- ctx$juncs[ctx$juncs$annotation_match]
spliceTerrain(ctx = ctx)
```

![](spliceTerrain_files/figure-html/ctx_plot-1.png)

The `annotation_match` field records whether each junction exactly
matches an intron implied by the supplied annotation. Plotting settings
can also be changed directly, for example with
`ctx$colours <- c("darkblue", "darkred")`. Settings that control BAM
import, filtering, or normalisation are not reapplied when a context is
replotted because its coverage and junctions are already processed.

Filtering `ctx$juncs` changes which arcs are displayed without changing
the usage denominator. Changing `ctx$junction_usage` to another
`GRanges` selection recalculates the fractions from `ctx$juncs_all` when
plotting. If you intend to change the reference evidence itself, edit
`ctx$juncs_all`, retaining one row per sample/junction and non-negative
`coverage_raw` counts. The new argument names are `junction_usage` and
`junction_usage_label_sep`. They replace the development names `psi` and
`psi_label_sep`. Recreate older cached contexts with the current API.

## References

## Session information

``` r

sessionInfo() |>
    pander()
```

**R Under development (unstable) (2026-10-12 r90666)**

**Platform:** x86_64-pc-linux-gnu

**locale:** *LC_CTYPE=C.UTF-8*, *LC_NUMERIC=C*, *LC_TIME=C.UTF-8*,
*LC_COLLATE=C.UTF-8*, *LC_MONETARY=C.UTF-8*, *LC_MESSAGES=C.UTF-8*,
*LC_PAPER=C.UTF-8*, *LC_NAME=C*, *LC_ADDRESS=C*, *LC_TELEPHONE=C*,
*LC_MEASUREMENT=C.UTF-8* and *LC_IDENTIFICATION=C*

**attached base packages:** *stats4*, *stats*, *graphics*, *grDevices*,
*utils*, *datasets*, *methods* and *base*

**other attached packages:** *GenomeInfoDb(v.1.48.0)*,
*pander(v.0.6.6)*, *scales(v.1.4.0)*, *EnsDb.Hsapiens.v75(v.2.99.0)*,
*ensembldb(v.2.36.1)*, *AnnotationFilter(v.1.36.0)*,
*GenomicFeatures(v.1.64.0)*, *AnnotationDbi(v.1.74.0)*,
*Biobase(v.2.72.0)*, *GenomicRanges(v.1.64.0)*, *Seqinfo(v.1.2.0)*,
*IRanges(v.2.46.0)*, *S4Vectors(v.0.50.3)*, *BiocGenerics(v.0.58.1)*,
*generics(v.0.1.4)*, *RNAseqData.HNRNPC.bam.chr14(v.0.50.0)*,
*spliceTerrain(v.0.99.0)* and *BiocStyle(v.2.40.0)*

**loaded via a namespace (and not attached):** *farver(v.2.1.2)*,
*blob(v.1.3.0)*, *Biostrings(v.2.80.2)*, *S7(v.0.2.2)*,
*bitops(v.1.1-0)*, *fastmap(v.1.2.0)*, *RCurl(v.1.98-1.20)*,
*lazyeval(v.0.2.3)*, *GenomicAlignments(v.1.48.0)*, *XML(v.3.99-0.25)*,
*digest(v.0.6.39)*, *lifecycle(v.1.0.5)*, *ProtGenerics(v.1.44.0)*,
*KEGGREST(v.1.52.2)*, *RSQLite(v.3.53.3)*, *compiler(v.4.7.0)*,
*rlang(v.1.3.0)*, *sass(v.0.4.10)*, *tools(v.4.7.0)*, *yaml(v.2.3.12)*,
*rtracklayer(v.1.72.0)*, *knitr(v.1.52)*, *labeling(v.0.4.3)*,
*S4Arrays(v.1.12.1)*, *bit(v.4.6.0)*, *curl(v.8.0.0)*,
*DelayedArray(v.0.38.2)*, *RColorBrewer(v.1.1-3)*, *abind(v.1.4-8)*,
*BiocParallel(v.1.46.0)*, *withr(v.3.0.3)*, *desc(v.1.4.3)*,
*grid(v.4.7.0)*, *ggplot2(v.4.0.3)*, *SummarizedExperiment(v.1.42.0)*,
*cli(v.3.6.6)*, *rmarkdown(v.2.32)*, *crayon(v.1.5.3)*, *ragg(v.1.5.2)*,
*otel(v.0.2.0)*, *httr(v.1.4.9)*, *rjson(v.0.2.23)*, *DBI(v.1.3.0)*,
*cachem(v.1.1.0)*, *parallel(v.4.7.0)*, *BiocManager(v.1.30.27)*,
*XVector(v.0.52.0)*, *restfulr(v.0.0.17)*, *matrixStats(v.1.5.0)*,
*vctrs(v.0.7.3)*, *Matrix(v.1.7-6)*, *jsonlite(v.2.0.0)*,
*bookdown(v.0.48)*, *patchwork(v.1.3.2)*, *bit64(v.4.8.6)*,
*systemfonts(v.1.3.2)*, *jquerylib(v.0.1.4)*, *glue(v.1.8.1)*,
*pkgdown(v.2.2.1)*, *codetools(v.0.2-20)*, *gtable(v.0.3.6)*,
*BiocIO(v.1.22.0)*, *UCSC.utils(v.1.8.0)*, *htmltools(v.0.5.9)*,
*R6(v.2.6.1)*, *textshaping(v.1.0.5)*, *evaluate(v.1.0.5)*,
*lattice(v.0.23-1)*, *png(v.0.1-9)*, *Rsamtools(v.2.28.0)*,
*cigarillo(v.1.2.1)*, *memoise(v.2.0.1)*, *bslib(v.0.12.0)*,
*Rcpp(v.1.1.2)*, *SparseArray(v.1.12.3)*, *xfun(v.0.61)*,
*pkgconfig(v.2.0.3)*, *fs(v.2.1.0)* and *MatrixGenerics(v.1.24.0)*

Garrido-Martín, Diego, Emilio Palumbo, Roderic Guigó, and Alessandra
Breschi. 2018. “ggsashimi: Sashimi Plot Revised for Browser- and
Annotation-Independent Splicing Visualization.” *PLOS Computational
Biology* 14 (8): e1006360.
<https://doi.org/10.1371/journal.pcbi.1006360>.

Hahne, Florian, and Robert Ivanek. 2016. “Visualizing Genomic Data Using
Gviz and Bioconductor.” In *Statistical Genomics: Methods and
Protocols*, edited by Ewy Mathé and Sean Davis. Springer New York.
<https://doi.org/10.1007/978-1-4939-3578-9_16>.

Yin, Tengfei, Dianne Cook, and Michael Lawrence. 2012. “ggbio: An R
Package for Extending the Grammar of Graphics for Genomic Data.” *Genome
Biology* 13 (8): R77. <https://doi.org/10.1186/gb-2012-13-8-r77>.
