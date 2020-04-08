Version 1.0-5
=============

* Remove dependence on asa.bst to comply with CRAN request.
* Processed vignette files (.Rnw, .R, .pdf) removed from doc/. 


Version 1.0-5
=============

* Update package documentation file to comply with CRAN policies.
* Update references in DESCRIPTION and pacakge documentation files.
* File 'ChangeLog' replaced with this file; previous contents copied over.

Version 1.0-4
=============

* Changes to DESCRIPTION file to comply with CRAN policies.

Version 1.0-3
=============

* Very minor changes to comply with new CRAN policies.

Version 1.0-2
=============

* Run time of tests reduced.

Version 1.0-1
=============

Sampler
-------

* thinning now implemented in sampler.
* mcmc objects output by sampler now contain start, end and thin values to match 'burn.in', 'n.iter' and 'thin.by' arguments.
* Created migration storage object twice (once for proportions, once for counts) but these were identical. Removed this.

Vignette
--------

* vignette modified so chain is of length 5E4, thinned by 50. Stored output is less than 5MB. LEB and total net number of migrants now calculated on the fly by vignette since the output is now very small. .RData files for these two parameters no longer produced---deleted.

Data Sets
---------

* Updates to the sampler and the CCMPP function require migration proportion inputs to be average ANNUAL proportions, not 5-year proportions.
* Tests being run with migration proportions divided by five to see if original results are reproduced.
