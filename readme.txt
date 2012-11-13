The script 'r_ras_mapper.R' can be used to plot hec-ras outputs in GIS form, just like ras mapper. It was written because bugs in ras-mapper can make it difficult to visualise the results of some simulations. It is slower than ras-mapper though, so should only be used if ras-mapper can't be made to work.

The script takes as input a hecras geometry file (*.gXX), and an XML file which is generated within hec-ras after a simulation is run, by using the 'File - Export Geometry and Results (RAS Mapper)'. The latter output option also makes another geometry file in GML format, which is not used by this script.  

See the script for more information. Set the input parameters to the right values, and go.
