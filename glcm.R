## Not run:
require(raster)
# Calculate GLCM textures using default 90 degree shift
textures_shift1 <- glcm(raster(L5TSR_1986, layer=1))
plot(textures_shift1)
# Calculate GLCM textures over all directions
textures_all_dir <- glcm(raster(L5TSR_1986, layer=1),
shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))
plot(textures_all_dir)
## End(Not run)
