
data:
  Rscript --vanilla ./R/data-analysis/data-analysis.R

report rep:
  quarto render {{rep}} --to pdf
  
  
  