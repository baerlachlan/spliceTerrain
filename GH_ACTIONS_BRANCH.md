# GitHub Actions Branch

This repository keeps `main` clean for Bioconductor submission. GitHub Actions
workflows live only on the `gh-actions` branch.

After committing changes to `main`, update and run the workflows with:

```bash
git switch gh-actions
git merge main
git push
```

The push to `gh-actions` triggers the coverage, R CMD check, and BiocCheck
workflows.

Do not merge `gh-actions` back into `main`, because that would copy
`.github/workflows/` into the Bioconductor-clean branch.
