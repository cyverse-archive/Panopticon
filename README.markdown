# Panopticon

A basic monitoring system for Condor. It alternates between calling condor_q and condor_history and updates the OSM with any analysis state changes that it detects.

Panopticon assumes that it is running as a user that can run the condor_q and condor_history commands. Those commands need to be on the PATH. Additionally, the handle_errors.sh script from filetool needs to be located in the /usr/local/bin/ directory. If you installed filetool via RPM, then it should already be there. In short, make sure filetool is installed on the same box as Panopticon.

