# sear 0.1.0

* First pre prod version of `sear`

* `manage_project` save project history in SQLite database. This database is also used to save `settings`.

* `settings` management for HOCR. Can enter settings, they will be saved automatically when the `Settings` menu item is closed. When re-opening the project, last saved settings are automatically re-loaded.

* `parse_data` raw data from MTE data logger and processed data from BioSonic. Also load and parse calibration data.

* `L1a_select` added support for filtering based on instrument data synthesis, `Date` and `Time`.

* `L1bL2` working module but struggle to manage reactivity like dynamic tab creation depending on available instrument data.

* `manage_obs` working save/update/fetch database observation module. Too rigid to my taste. Could improve this by better mastering reactivity (linked to `L1bL2`).

* `L2_select` demonstration of database explorer. For now mainly useful for $R_{rs}$ 

# sear 0.0.0.9002

* Add dark correction to HOCR L1b. The function `cal_dark` is almost a duplicate of `cal_hocr` with the exception of interpolation time interval set to 60 sec and returning a wide data frame of calibrated dark. Could improve the workflow by avoiding duplication of code ...

* Fix map display bug when saving new data without UUID. In that case Obs$Metadata is updated after the Center and Zoom values are updated which was creating a bug. Remove the constraint of having a valid UUID. Does not seem to be a problem since a valid UUID is created when `save` is hit.

# sear 0.0.0.9001

* Abandon the raw -> station/transect logic
* Adopt the raw -> discrete obs logic, changed names accordingly

# sear 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
