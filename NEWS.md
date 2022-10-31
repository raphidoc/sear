# sear 0.0.0.9002

* Add dark correction to HOCR L1b. The function `cal_dark` is almost a duplicate of `cal_hocr` with the exception of interpolation time interval set to 60 sec and returning a wide data frame of calibrated dark. Could improve the workflow by avoiding duplication of code ...

* Fix map display bug when saving new data without UUID. In that case Obs$Metadata is updated after the Center and Zoom values are updated which was creating a bug. Remove the constraint of having a valid UUID. Does not seem to be a problem since a valid UUID is created when `save` is hit.

# sear 0.0.0.9001

* Abandon the raw -> station/transect logic
* Adopt the raw -> discrete obs logic, changed names accordingly

# sear 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
