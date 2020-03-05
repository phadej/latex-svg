.PHONY : output.html

output.html :
	cabal build latex-svg-filter
	pandoc -s --metadata pagetitle=demo --filter=$$(cabal-plan list-bin latex-svg-filter) -o output.html input.markdown
