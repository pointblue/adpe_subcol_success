import arcpy
import numpy as np
import numpy.ma as ma
import os
import math

#os.chdir("C:/YiranResearch/DEMs")
os.chdir("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers")
#DEMin = os.path.join(os.getcwd(),"smallertestdem.tif")
DEMin = os.path.join(os.getcwd(),"crozier_mosaic_dem-tile-0_clip.tif")
TRIout = os.path.join(os.getcwd(),r"croz_TRI.tif")
#DEMin = arcpy.GetParameterAsText(0)
#TRIout = arcpy.GetParameterAsText(1)
winsize = 3
#winsize = int(arcpy.GetParameterAsText(2))

try:
    demRaster = arcpy.Raster(DEMin)
    arcpy.env.overwriteOutput = True
    arcpy.env.outputCoordinateSystem = DEMin
    arcpy.env.cellsize = DEMin

    rad = int(winsize/2)
    demcols = demRaster.width
    demrows = demRaster.height

    mx = demRaster.extent.XMin
    my = demRaster.extent.YMin

    arcpy.AddMessage("Reading the input DEM...")
    demArray = arcpy.RasterToNumPyArray(demRaster,arcpy.Point(mx,my),demcols,demrows)
    maskDEMArray = ma.masked_where(demArray == -9999.0, demArray)

    newshape = (int(np.ceil(demrows/winsize)), int(np.ceil(demcols/winsize)))
    TRIArray = np.zeros(newshape)

    arcpy.AddMessage("Calculating TRI index...")
    trirow = 0
    for i in range(rad,demrows-rad,winsize):
        tricol = 0
        for j in range(rad,demcols-rad,winsize):
            diff = 0.0
            if maskDEMArray[i, j] is ma.masked:
                TRIArray[trirow, tricol] = -9999.0
            else:
                winArray = maskDEMArray [i-rad:i+rad+1, j-rad:j+rad+1]
                diff = np.sum((winArray - maskDEMArray[i, j]) ** 2)
                TRIArray[trirow, tricol] = math.sqrt(diff)
                #print (diff)
            tricol = tricol+1
        trirow = trirow+1

    arcpy.AddMessage("Saving the TRI index map...")
    TRIRaster = arcpy.NumPyArrayToRaster(TRIArray,arcpy.Point(mx,my),winsize*demRaster.meanCellWidth,winsize*demRaster.meanCellHeight,-9999.0)
    TRIRaster.save(TRIout)

    del TRIRaster
    del demRaster
    arcpy.AddMessage("All done!")
except arcpy.ExecuteError:
    print(arcpy.GetMessages())