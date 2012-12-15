## Code which attempts to parse some .xml hec-ras output into R
## and use these to make a ras-mapper style plot
## This could potentially be used avoid the bugs in RAS-Mapper

# Input vars
ras_xml='pasig_sanjuan.O11.xml'#'test.O04.xml'
ras_geo='pasig_sanjuan.g18' #'test.g01'
#lidar_dem_file='C:/Users/Gareth/Documents/work/docs/Nov_2011_workshops/qgis/LIDAR_and_IMAGERY/DEM/10m_DEM/test2_10m.tif'
lidar_dem_file='../../Nov_2011_workshops/qgis/LIDAR_and_IMAGERY/DEM/10m_DEM/test2_10m.tif'
lidar_vertical_offset=10.5

output_raster_file='water_surface_pasig3.tif'
output_depth_file='water_depth_pasig3.tif'

time='Max WS'


## Main code

source('hec_help_util.R') # Functions for working with hec-ras geometry
library(XML)

print('Parsing xml ...')
doc=xmlParse(ras_xml)

## Extract relevant bits from XML doc

# For xsections, get the River, Reach, RiverStation, and Profile corresponding to time
#xs_xml=getNodeSet(doc, paste("//xs/River | //xs/Reach | //xs/RiverStation | //xs/Profile[Name='", time, "']", sep=""))
print('Getting XML Nodes ...')
xs_river=unlist(getNodeSet(doc, "//xs/River", fun=xmlToList))
xs_reach=unlist(getNodeSet(doc, "//xs/Reach", fun=xmlToList))
xs_riverstation=unlist(getNodeSet(doc, "//xs/RiverStation", fun=xmlToList))
xs_profile=getNodeSet(doc, paste("//xs/Profile[Name='", time, "']", sep=""), fun=xmlToList)

# Extract the water surface for each xsection
tmp=unlist(xs_profile)
tmp= tmp[ names(tmp)=='WSE']
tmp=unlist( strsplit(tmp, ",") )
water_surface_elevs=as.numeric(tmp[ seq(2, length(tmp), by=2)])

# For storage areas, get the Name, and Profile corresponding to time
#sa_xml=getNodeSet(doc, paste("//sa/Name | //sa/Profile[Name='", time, "']", sep=""))
sa_name=getNodeSet(doc, '//sa/Name', fun=xmlToList)
sa_profile=getNodeSet(doc, paste("//sa/Profile[Name='", time, "']", sep=""), fun=xmlToList)

print('Getting raster data ...')

####### Raster stuff

library(raster)
dem=raster(lidar_dem_file)
burn_rast=raster(dem) # Blank raster with res / projection / extent from dem

spatial_proj=dem@crs@projargs

####### Read Hecras .g0x geometric data

print('Reading in ras geometry ...')
library(rgeos)
ras_storage=get_existing_storage_areas(ras_geo, spatial_proj)
ras_xsections=make_channel_cutlines(ras_geo, spatial_proj)
ras_chan_polygon=create_channel_polygon(ras_geo, CRS(spatial_proj))
ras_junction_polygon=create_junction_polygon(ras_geo, ras_xsections)
#stop()

#FIXME: ADD CODE FOR 'create_junction_polygon'. Idea: Connect each upstream reach to every downstream reach individually, then union. This polygon can be the junction.

# Extract reach/river/station from xsections -- as we will need this to 
# ensure the order of xsections in the xml output file is the same as 
# the order of xsections in ras_xsections
ras_xsect_reach=substring(ras_xsections@data$reach, 13, 13+16-1) #  Reach name
ras_xsect_reach=sub(' +$', '', ras_xsect_reach) # cut trailing whitespace
ras_xsect_river=substring(ras_xsections@data$reach, 14+16, 14+16+16-1) # River name
ras_xsect_river=sub(' +$', '', ras_xsect_river) # cut trailing whitespace
ras_xsect_station=substring(ras_xsections@data[,2], 28, 28+8-1) # Station name
ras_xsect_station=sub(' +$', '', ras_xsect_station) # cut trailing whitespace

# Check that the order of ras_storage = order of storage areas [experience
# suggests it does, unsurprising as both come from files written by hecras]
c1=as.character(unlist(sa_name))
c2=gsub(" ", "", as.character(ras_storage@data[,1]) )
if(!all(c1==c2)){
    print('Q: Do the .xml and the .gXX file correspond ??')
    stop('ERROR: Order of storage areas in xml and ras_geo appears to differ')
}


# Force order of xsectional data from xml file to be the same as order of
# xsections in ras_xsections. These are typically different
xml_data=paste(xs_river, xs_reach, xs_riverstation)
ras_geo_data=paste(ras_xsect_reach, ras_xsect_river, ras_xsect_station)

if(length(xml_data)!=length(ras_geo_data)){
    print('Q: Do the .xml and the .gXX file correspond ??')
    print('They seem to have different numbers of xsections')
    stop('ERROR: Problem in matching the xml indices with the ras_geo_data')
}

# Find re-ordering of xml_data to match ras_geo_data
ras_xml_indices=unlist(lapply(ras_geo_data, function(x) which(xml_data == x) ))
if(min(range(ras_xml_indices)!=1)){
    print('Q: Do the .xml and the .gXX file correspond ??')
    print(' I could not make a nicely behaved mapping between them')
    stop('ERROR: Problem in matching the xml indices with the ras_geo_data')
}

    
# Re-order XML data
xs_river=xs_river[ras_xml_indices]
xs_reach=xs_reach[ras_xml_indices]
xs_riverstation=xs_riverstation[ras_xml_indices]
water_surface_elevs=water_surface_elevs[ras_xml_indices]

##################################################################

# Get storage area water elevations
sa_prof2=unlist(sa_profile)
sa_we=as.numeric(sa_prof2[ names(sa_prof2)=='WSE' ])

print('Burning storage areas into raster (progress bar can advance at a variable rate) ...')
burn_rast=rasterize(ras_storage, burn_rast, sa_we, progress='text', update=TRUE)

# Get junction water elevations into raster
junct_we = water_surface_elevs[ ras_junction_polygon@data[,2] ]

print('Burning junctions into raster (progress bar can advance at a variable rate) ...')
burn_rast=rasterize(ras_junction_polygon, burn_rast, junct_we, progress='text', update=TRUE)


print(' Getting points from the raster which are in the channel polygon ...')
chan_cells=extract(dem, ras_chan_polygon, cellnumber=TRUE, small=TRUE)

library(rgeos)
elev_fun<-function(coords, reachname){
    # Functions to compute water surface elevation for points in channel
    # This will be burned into the raster

    reach_xsection_inds=which(ras_xsections@data$reach==reachname)
    reach_xsections=ras_xsections[reach_xsection_inds,]

    # Get the water surface elevations for these xsections
    local_water_surface=water_surface_elevs[reach_xsection_inds]
    #browser()
    
    # Make polygons of consecutive xsections for each reach
    reach_polys=list()
    for(i in 1:(length(reach_xsection_inds)-1) ){
        cc1=coordinates(reach_xsections[i,])[[1]][[1]]
        cc2=coordinates(reach_xsections[i+1,])[[1]][[1]]
        # Flip the order of cc2, so we can make the polygon
        cc2 = cc2[length(cc2[,1]):1, 1:2]

        poly_t1=Polygons( list( Polygon(rbind(cc1, cc2, cc1[1,]) ) ), ID=as.character(i) )
        reach_polys[[i]] = poly_t1
    
    }
    # SpatialPolygons of the reach
    poly_t2=SpatialPolygons( reach_polys, proj4string=CRS(spatial_proj))

    # SpatialPoints of the coordinates
    points_t2 = SpatialPoints(coords, proj4string=CRS(spatial_proj))

    reach_seg=over(points_t2, poly_t2)

    # Treat rare NA values in reach_seg -- cells treated as outside by 'over',
    # but inside by 'extract
    fixme=which(is.na(reach_seg))
    if(length(fixme)>0){
        reach_seg[fixme]=over(gBuffer(points_t2[fixme],width=sqrt(50.)/2.), poly_t2)
    }
    #browser()
    # Simple computation of water surface elevation
    output=(local_water_surface[reach_seg] + local_water_surface[reach_seg+1])/2
    #browser()
    # Improve accuracy
    for(i in 1:max(reach_seg)){
        # Weighted average of the distances to each point
        inds=which(reach_seg==i)
        d1=gDistance(points_t2[inds,], reach_xsections[i,], byid=T)
        d2=gDistance(points_t2[inds,], reach_xsections[i+1,], byid=T)
        output[inds] = (d2*local_water_surface[i]+d1*local_water_surface[i+1])/(d1+d2)
    }

    return(output)
}


# Burn each channel into the raster
reach_names=as.character(unique(ras_xsections@data$reach))
reach_xyz=c()
for(i in 1:length(chan_cells)){
    print(paste('Computing channel ', i))
    reach_name=reach_names[i]
    coords=xyFromCell(dem, chan_cells[[i]][,1])
    #burn_rast[chan_cells[[i]][,1]] = elev_fun(chan_cells[[i]][,1], reach_name)
    coords_z=elev_fun(coords, reach_name)
    reach_xyz=rbind(reach_xyz, cbind(coords, coords_z))
}
print('Burning channel into raster')
burn_rast = rasterize(reach_xyz[,1:2], burn_rast, reach_xyz[,3], update=TRUE, progress='text')
gc()

# Here, we really need to add a gdal_fillnodata.py water_buf.tif -md 2
# out_buf.tif to clean things up.  Alternatively, can do this later in batch,
# followed by batch gdal_calc.py to do the subtraction

print('Writing the raster to a geotiff ...')
writeRaster(burn_rast, output_raster_file,'GTiff', overwrite=TRUE) 

print('Computing depth ...')
depthfun<-function(water_level, dem){
    depth=(water_level-(dem+ lidar_vertical_offset))
    depth=depth*(depth>0.)
    return(depth)
}

depthrast= overlay(burn_rast, dem, fun=depthfun)
writeRaster(depthrast, output_depth_file, 'GTiff', overwrite=TRUE)


print('Done <:)>')
