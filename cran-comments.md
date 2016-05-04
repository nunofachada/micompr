## Test environments
* Ubuntu 16.04 R 3.2.5
* Windows 8.1 R 3.2.5

## R CMD check results
There were no ERRORs, WARNINGs or NOTES.

There was Warning from roxygen2:

1: @examples [latex_tables.R#63]: mismatched braces or quotes 
2: @examples [latex_tables.R#151]: mismatched braces or quotes 

This is a bug in roxygen2 5.x which does not allow comments in doc-comments.

## Downstream dependencies
