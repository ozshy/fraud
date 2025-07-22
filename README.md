# fraud
Derive and R files for a paper titled: "Whistleblowers and Financial Fraud"

by Oz Shy

https://papers.ssrn.com/abstract=5121886

*** How Table 1 was constructed (Appendix A in the paper) ***

The spreadsheet sec_awards_2025_mm_dd.xlsx provides the raw data that I collected manually from 133 separate SEC press releases during 2012-2024. It also contains links to each of the 133 press releases. 

The R-code sec_awards_2025_mm_dd.R inputs the spreadsheet and generates Table 1 in LaTeX format. Don't forget the change the working directory to the directory where the R-code and the Excel file are placed. 


*** R-code for the numerical simulations: fraud_2025_mm_dd.R ***

Run the entire code first (don't forget to change the working directory). 

The first part simulates Figure 5 in the paper (just for fun, not really needed). 

The second part simulates Result 4(a) showing a case where the optimal compensation is C_min and a case where the optimal compensation is C_max. 

*** Other files in the repository (not related to Table 1) ***

All algebraic derivations for the above article (not related to data) can be found on "fraud_2025_mm_dd.dfw.pdf" (which the reader can download). The reader can then scroll down to the desired derivation using the same section, subsection, and equation numbers correspoding to the paper itself.

The above PDF file was constructed using a symbolic algebra software called "Derive for Windows." The file "fraud_2025_mm_dd.dfw" requires this sofware (which most people don't have). Therefore, it is sufficient  to download only the PDF.


