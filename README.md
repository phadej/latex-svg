# LaTeX SVG

This is a fork for [`latex-formulae`](https://github.com/liamoc/latex-formulae)
which produces SVG images instead of raster PNGs.

Some differences:

- Uses `latex` and `dvisvgm`, ImageMagick *is not required*.  (You don't need to tweak `policy.xml`)
- `latex-svg-image` supports global cache (off by default), which speedups hakyll site builds.
- `latex-svg-hakyll` has `initFormulaCompilerSVGPure` variant, which doesn't need `IO` to be created. You don't need to thread `renderFormulae` function through.
- Drawback is that result pages become bigger. For example a formula-heavy https://oleg.fi/gists/posts/2018-12-12-find-correct-laws.html is

    - `576k` in size with `latex-formulae`
    - `2819k` in size with `latex-svg`

  Also rendering SVG images is more expensive.
