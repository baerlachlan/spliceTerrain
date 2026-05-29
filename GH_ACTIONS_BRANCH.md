# GitHub Actions Branch

This repository keeps `devel` clean for Bioconductor submission. GitHub
Actions workflows live only on the `gh-actions` branch.

After committing changes to `devel`, update and run the workflows with:

``` bash
git switch gh-actions
git merge devel
git push
```

The push to `gh-actions` triggers the coverage, R CMD check, BiocCheck,
and pkgdown workflows. The pkgdown workflow deploys the rendered site to
the `gh-pages` branch.

Do not merge `gh-actions` back into `devel`, because that would copy
`.github/workflows/` into the Bioconductor-clean branch.
