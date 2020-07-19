# Sensors

Each sensor is backed up by at least one redundant sensor. So, in case that the primary sensor fails, the backup sensor is used, making the system fault tolerant.

## Front Distance
Front distance sensors are used to detect obstacles in front of the cab. They trigger a function that stops the cab.
### Ultrasonic
#### Primary Sensors
- dist_l : front distance ultrasonic sensor left primary
- dist_c : front distance ultrasonic sensor center primary 
- dist_r : front distance ultrasonic sensor right primary 
#### Backup Sensors
- dist_l2 : front distance ultrasonic sensor left backup 
- dist_c2 : front distance ultrasonic sensor center backup 
- dist_r2 : front distance ultrasonic sensor right backup 

### Infrared

#### Primary Sensors

- dist_ir_l : front distance infrared sensor left primary 
- dist_ir_c : front distance infrared sensor center primary 
- dist_ir_r : front distance infrared sensor right primary 

#### Backup Sensors

- dist_ir_l2 : front distance infrared sensor left backup 
- dist_ir_c2 : front distance infrared sensor center backup 
- dist_ir_r2 : front distance infrared sensor right backup 

## Road Marker Detection

The road markers are used to locate the cab on the track.

### Outer Sensors

The outer road marker sensors trigger the reading of the inner sensor values. When all four outer roadmarker sensors give a positive signal the reading of the inner sensors is triggered.

#### Primary Sensors

- inf_rm_fl_act : road marker sensor front left trigger primary
- inf_rm_fr_act : road marker sensor front right trigger primary
- inf_rm_bl_act : road marker sensor back left trigger primary
- inf_rm_br_act : road marker sensor back right trigger primary

#### Backup Sensors

- inf_rm_fl_act2 : road marker sensor front left trigger backup
- inf_rm_fr_act2 : road marker sensor front right trigger backup
- inf_rm_bl_act2 : road marker sensor back left trigger backup
- inf_rm_br_act2 : road marker sensor back right trigger backup

### Inner Sensors
The inner road marker sensors are reading the road marker value, when the reading is triggered by the outer sensors. The identification of road markers is based on a binary system.

#### Primary Sensors

- inf_rm_fl : road marker sensor front left value1 primary
- inf_rm_fr : road marker sensor front left value2 primary
- inf_rm_bl : road marker sensor front left value4 primary
- inf_rm_br : road marker sensor front left value8 primary

#### Backup Sensors

- inf_rm_fl2 : road marker sensor front left value1 backup
- inf_rm_fr2 : road marker sensor front left value2 backup
- inf_rm_bl2 : road marker sensor front left value4 backup
- inf_rm_br2 : road marker sensor front left value8 backup

## Lane Detection

The Lane can be detected through two independent methods. On one hand the middle line is detected by infrared sensors. On the other hand the curbs are detected by ultrasonic sensors.

### Middle Line Detection

The middle line is detected by monitoring the reflection of the ground. Due to a low roughness the middle line reflects very good. The rest of the ground has a high roughness, which leads to a low reflection.

#### Primary Sensors

- inf_left : infrared line detection left primary
- inf_cent : infrared line detection center primary
- inf_right: infrared line detection right primary

#### Backup Sensors

- inf_left2 : infrared line detection left backup
- inf_cent2 : infrared line detection center backup
- inf_right2 : infrared line detection right backup

### Curb Detection

The Lane is surrounded by curbs which are detected by ultrasonic sensors. By measuring the distance to these curbs the cab stays on track.

#### Primary Sensors

- curb_lf : ultra sonic curb detection left front primary
- curb_rf : ultra sonic curb detection right front primary

#### Backup Sensors

- curb_lf2 : ultra sonic curb detection left front backup
- curb_lr2 : ultra sonic curb detection right front backup