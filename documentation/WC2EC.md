# WC2EC - Webots Controller to external Controller 
# Be CAREFULL this is a FIRST DRAFT do NOT rely on it, yet.

## Header
|  Bits             |      Name                    | Description|
|----------------|----------|-----------------------|
|8| Command 			  | Action of this command|
|8| Sensor Type       	  | Determines the type of the sensor |
|8|Sensor Number          | Number of the Sensor            |

We refrained from adding a Checksum or Package length to the header, since:
The protocol is build ONTOP if TCP, which already provides: 
Order, Checksum and Retransmission
The Packagelength is also not necessary in the current implementation, since it is given implicit by the combination of command and sensor type.

### Commands
The range for the command is split in half, first 0-127 is reserved for the Webots Controller, 128 - 255 is reserved for the External Controller. At reception of a package a range check SHOULD be performed. 
#### Webots Controller -> External Controller
|  Number             |      Name                    | Description|
|----------------|----------|-----------------------|
|0| sensorData| Used by the Webotscontroller to send data to the The Package contains data measure by the sensors specified in 'Sensor Type' and 'Sensor Number'|
| 1- 127 | reserved | Reserved for future use
#### External Controller -> Webots Controller
|  Number             |      Name                    | Description|
|----------------|----------|-----------------------|
|128| enableSensor | Enables the sensor specified in 'Sensor Type' and 'Sensor Number' |
|129| disableSensor|  Disables the sensor specified in 'Sensor Type' and 'Sensor Number' |
|130| changeSensor | Changes a single configuration value the sensor specified in 'Sensor Type' and 'Sensor Number' The first byte of the payload indicates which sensor value will be changed |
|131| setSensor | Sets all configuration values for the sensor specified in 'Sensor Type' and 'Sensor Number' |
|132 | reserved | Resered for future use |

### Sensor Types
Currently only the sensors needed for our project are implemented. Those who feel themself called, feel free to extend this protocol!
|  Number             |      Name                    | Description|
|----------------|----------|-----------------------|
| 0 | LightSensor | returns the most recent value measured by the specified light sensor. The returned value is the result of interpolating the irradiance| 


### Sensor Details 
#### Light Sensor
Type Number: 0
Sensor Data Length: 8 Byte
Sensor Data Layout: double

