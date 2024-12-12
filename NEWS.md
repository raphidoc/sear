# sear 0.4.2

# sear 0.4.1

* Fix bug for Applanix Trame being NA in `read_apla`

* Fix bug for SeaOWL serial number being NA in `cal_seaowl`

# sear 0.4.0

* Add parsing and processing support for HydroBall Devices as MainLog, NMEA string Depth Below Transducer (DBT) as water column height

* Add parsing of  HOCR SatView raw files.

* Improving error handling in reading HOCR binary Files

* Violating Satlantinc standards, sensor HSL and HLD should NOT be $L_u$. Case in WISEMan 2019.

* Adding vessel frame rotation from Tait-Bryan angles heading, pitch, roll in metadata_l1b and L2

# sear 0.3.0

# sear 0.2.0

* Added `Rrs_loess` and `RbI` computation and storage in SQLite `hocr_l2` table with DB migration script in `sear_dev`.

* Lowered computation time of `data_synthesis` by passing time vector unique to the second.

# sear 0.1.0.9

* Added `KLu_loess` smoothing and SQLite DB entry with DB migration script in `sear_dev`.

* Added proper validation of `SelID()` in `L1a_select`.

# sear 0.1.0

* First pre prod version of `sear`

* `manage_project` save project history in SQLite database. This database is also used to save `settings`.

* `settings` management for HOCR. Can enter settings, they will be saved automatically when the `Settings` menu item is closed. When re-opening the project, last saved settings are automatically re-loaded.

* `parse_data` raw data from MTE data logger and processed data from BioSonic. Also load and parse calibration data.

* `L1a_select` added support for filtering based on instrument data synthesis, `date` and `time`.

* `L1bL2` working module but struggle to manage reactivity like dynamic tab creation depending on available instrument data.

* `manage_obs` working save/update/fetch database observation module. Too rigid to my taste. Could improve this by better mastering reactivity (linked to `L1bL2`).

* `L2_select` demonstration of database explorer. For now mainly useful for $R_{rs}$ 

# sear 0.0.0.9002

* Add dark correction to HOCR L1b. The function `cal_dark` is almost a duplicate of `hocr_l1b` with the exception of interpolation time interval set to 60 sec and returning a wide data frame of calibrated dark. Could improve the workflow by avoiding duplication of code ...

* Fix map display bug when saving new data without uuid_l2. In that case Obs$Metadata is updated after the Center and Zoom values are updated which was creating a bug. Remove the constraint of having a valid uuid_l2. Does not seem to be a problem since a valid uuid_l2 is created when `save` is hit.

# sear 0.0.0.9001

* Abandon the raw -> station/transect logic
* Adopt the raw -> discrete obs logic, changed names accordingly

# sear 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
