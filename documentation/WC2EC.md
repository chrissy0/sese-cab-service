# WC2EC - Webots Controller to external Controller 
The information provided here are still subject to change.

# Header

<table border="1">
	<tr>
		<td><b>Byte</b></td>
		<td colspan="8">0</td>
		<td colspan="8">1</td>
		<td colspan="8">2</td>
	</tr>
	<tr>
		<td><b>Name</b></td>
		<td colspan="8"><i>Command</i></td>
		<td colspan="8"><i>Sensor Type</i></td>
		<td colspan="8"><i>Sensor ID</i></td>
	</tr>
	<tr>
		<td><b>Description</b></td>
		<td colspan="8"><i>Action of this Command</i></td>
		<td colspan="8"><i>Determines the sensor type</i></td>
		<td colspan="8"><i>Sensor ID</i></td>
	</tr>
</table>

We refrained from adding a Checksum or Package length to the header, since:
The protocol is build ONTOP of TCP, which already provides: 
order, checksum, retransmission and robustness
A package length field is also not necessary in the current implementation, since it is given implicit by the combination of Command and Sensor Type.

## Commands
The range for the command is split in half, first 0-127 is reserved for the Webots Controller, 128 - 255 is reserved for the External Controller. At reception of a package a range check SHOULD be performed. 
# Webots Controller -> External Controller

<table border="1">
	<tr>
		<td><b>Byte</b></td>
		<td colspan="8">0</td>
		<td colspan="8">1</td>
		<td colspan="8">2</td>
		<td colspan="8">3</td>
		<td colspan="1">...</td>
	</tr>
	<tr>
		<td colspan="1"></td>
		<td colspan="24"><b><center>HEADER</center></b></td>
		<td colspan="9"><b><center>BODY ...</center></b></td>
	</tr>
	<tr>
		<td><b>Name</b></td>
		<td colspan="8"><i>Command</i></td>
		<td colspan="8"><i>Sensor Type</i></td>
		<td colspan="8"><i>Sensor ID</i></td>
		<td colspan="9"><i>Sensor Data...</i></td>
	</tr>
</table>

<i>Sensor Type</i>:
<ul>
	<li><b>0x01</b>: Camera</li>
	<li><b>0x02</b>: Light Sensor</li>
	<li><b>0x03</b>: Distance Sensor</li>
	<li><b>Other</b>: Unused</li>
</ul>

<h3>Sensor Package Types</h3>
<table border="1">
	<tr>
		<td><b>Sensor Type</b></td>
		<td><b>Package Body Size</b></td>
		<td><b>Description</b></td>
		<td><b>Documentation</b></td>
	</tr>
	<tr>
		<td><i>Camera</i></td>
		<td>camera_width * camera_height * 4 byte</td>
		<td>Full RGB camera image. Value from <i>wb_camera_get_image()</i></td>
		<td><a href="https://cyberbotics.com/doc/reference/camera#wb_camera_get_image">here</a></td>
	</tr>
	<tr>
		<td><i>Distance Sensor</i></td>
		<td>8 byte <i>(Double)</i></td>
		<td>Value from  <i>wb_distance_sensor_get_value() </i></td>
		<td><a href="https://cyberbotics.com/doc/reference/distancesensor#wb_distance_sensor_get_value">here</a></td>
	</tr>
	<tr>
		<td><i>Light Sensor</i></td>
		<td>8 byte <i>(Double)</i></td>
		<td>Value from  <i>wb_light_sensor_get_value() </i></td>
		<td><a href="https://cyberbotics.com/doc/reference/lightsensor#wb_light_sensor_get_value">here</a></td>
	</tr>
</table>
<h3>Usage</h3>
<ul>
	<li>Read header (first three byte)</li>
	<li>Evaluate sensor type and ID</li>
	<li>Get package body using the specified package body size</li>
</ul>

|  Number             |      Name                    | Description|
|----------------|----------|-----------------------|
|0| sensorData| Used by the Webotscontroller to send data to the The Package contains data measure by the sensors specified in 'Sensor Type' and 'Sensor Number'|
| 1 - 126 | reserved | Reserved for future use |
| 127 | Error | The sensor specified in 'Sensor Type' and 'Sensor Number' encountered an error |
# External Controller -> Webots Controller
|  Number             |      Name                    | Description|
|----------------|----------|-----------------------|
|128| enableSensor | Enables the sensor specified in 'Sensor Type' and 'Sensor Number' |
|129| disableSensor|  Disables the sensor specified in 'Sensor Type' and 'Sensor Number' |
|130| changeSensor | Changes a single configuration value the sensor specified in 'Sensor Type' and 'Sensor Number' The first byte of the payload indicates which sensor value will be changed |
|131| setSensor | Sets all configuration values for the sensor specified in 'Sensor Type' and 'Sensor Number' |
|132 - 255 | reserved | Resered for future use |

## Sensor Types
|  Number    |      Name              | Description|
|----------------|----------|-----------------------|
| 0 | LightSensor | returns the most recent value measured by the specified light sensor. The returned value is the result of interpolating the irradiance| 

Currently only the sensors needed for our project are implemented. Those who feel themself called, feel free to extend this protocol!

## Sensor Details 
### Light Sensor
Type Number: 0
Sensor Data Length: 8 Byte
Sensor Data Layout: double

