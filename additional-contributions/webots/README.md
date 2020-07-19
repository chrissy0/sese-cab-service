# Old Webots worlds
## Disclaimer

There are two versions of the cab in this folder.

Version 1 is a state at the beginning of the project that cab has the ability to follow the white
lane and to recognize objects with camera. At this time, we tried to use the camera, but in
our system, we decided that cab could work well without the camera, so we removed the
camera function.

Version2 was in the state of the cab before we moved the algorithm to ADA (GNAT). Features
such a s Road marker, Lane Following, Front Distance, into the Depot, out to the
Depot have already been added. Based on the statechart and "these algorithms" , we
started developing the external controller in ADA.

## Setup

- Install Webots
- Navigate to `version1/worlds` or `version2/worlds`
- Open the file `cab.wbt`
- In the Webots program go to `File -> Open Text File`
- Choose controller by navigating to:
  - for version1: `version1/controllers/my_controller/my_controller.cpp`
  - for version2: `version2/controllers/controller/controller.cpp`
- Click on the right hand side on the gear wheel or press F7 to build the Controller
- Click the `reset` button







