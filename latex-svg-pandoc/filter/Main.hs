import Image.LaTeX.Render.Pandoc
import Image.LaTeX.Render
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter $ convertFormulaSVG env fopts
  where
    env = defaultEnv { latexFontSize = 14 }
    fopts = defaultPandocFormulaOptions
        { formulaOptions = \mt -> (formulaOptions defaultPandocFormulaOptions mt)
            { preamble = unlines
                [ "\\usepackage{amsmath}"
                , "\\usepackage{amsfonts}"
                , "\\usepackage[T1]{fontenc}"
                , "\\usepackage{lmodern}"
                , "\\usepackage{xcolor}"
                ]
            }
        }
