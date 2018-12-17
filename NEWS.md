
# swissrastermaps4r 0.1.2

* Added the possibility of adding ".pattern"-Files in each folder to get metadata from each filename
* `init_fdir()` now reads file size
* Added `fn_year_start` and `fn_year_end`
* Can now handle different EPSG-Codes
* Resolve the issue with naming the 1:1'000'000 files (also allow different representations of the same map)
* added `guess_scale()` function that guesses the scale based on the input
* when overlapping raster exist, the packge tries filtering futher over year and then name
* Renamed sample data
* Breaking change: Removed the `per_feature` option
* started modularizing the code
* Fixed a lot of bugs


# swissrastermaps4r 0.1.1

* Added documentation with pkgdown
* A minimal version now works
* Added a `NEWS.md` file to track changes to the package.


# swissrastermaps4r 0.1.0

* This is the first release of pkgdown. It doesn't really work yet, but all scripts are now on github
