# since I dont know how to work with branches just yet and I dont want to commit
# all my breaking changes, I'll just write down my progress here.
#
# I've started on implementing a coulple of things simultaniously
#
# - handle multiple EPSG
# - get metadata from filesnames by specifying a .pattern
#
# Currently, "show_extens()" is broken and needs to be fixed
# Also, the last values in the folder names is now not the name, but an index
#     --> document this in the docs
#     --> resolve the issue with the 1:1'000'000 files
# Multiple CRS must allow multiple fdir files (?)
# multiple CRS must include the crs of the feature in the get_raster() function?
#
# todo: update all the docs
# learn to make branches
# implement the possiliblty to add multiple rootdirs in init_fdir()
