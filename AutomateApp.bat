@echo off
C:
PATH "C:\Program Files\R\R-4.4.1\bin\x64"
cd C:\Users\mcard\OneDrive\Documents\MAIA\Shiny\CEAMS\code
Rscript process_ceams_data.R && Rscript republish.R
