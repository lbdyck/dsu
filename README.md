# Dataset Utility ISPF Application

DSU - the Data Set Utility is an ISPF dialog that is designed to be
invoked from the ISPF 3.4 (DSList) list of datasets as a selection
command.  It will take the allocation information for the selected
dataset allowing the user to use that allocation, or change it, to
allocate a new dataset. The selected dataset will then be copied to the
new dataset.

If the selected dataset is a PDSE with member generations then the
PDSEGEN package is utilized to copy the members,  with all generations,
to the new dataset.

This package comes with no guarantees, warranties, or other promises.
