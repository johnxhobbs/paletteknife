## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* autocol: no visible binding for '<<-' assignment to
  autolegend: no visible binding for global variable
    '.autolegend_persistent'
  create_autolegend_data: no visible binding for '<<-' assignment to
    '.autolegend_persistent'
    '.autolegend_persistent'
  Undefined global functions or variables:
    .autolegend_persistent
  
  For convenience, autocol(x) creates a small object '.autolegend_persistent' in 
  its parent scope. This allows the associated legend to be drawn later without
  intermediate steps by the user. A hidden but documented object is the most
  elegant solution for this tool which is intended for interactive use.
  
* Note, this is a first submission

## Downstream dependencies
There are currently no downstream dependencies for this package
