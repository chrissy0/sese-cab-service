# WC2EC - Webots Controller to external Controller 
The information provided here are still subject to change.

# Header

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

We refrained from adding a Checksum or Package length to the header, since:
The protocol is build ONTOP of TCP, which already provides: 
order, checksum, retransmission and robustness
A package length field is also not necessary in the current implementation, since it is given implicit by the combination of Command and Sensor Type.

## Commands
The range for the command is split in half, first 0-127 is reserved for the Webots Controller, 128 - 255 is reserved for the External Controller. At reception of a package a range check SHOULD be performed. 
### Webots Controller -> External Controller 
|  Number             |      Name                    | Description|
|----------------|----------|-----------------------|
|0| registerSensor| Used by the Webotscontroller to send the sensorname and the assigned sensor ID to the External Controller|
|1| sensorData| Used by the Webotscontroller to send data to the The Package contains data measure by the sensors specified in 'Sensor Type' and 'Sensor Number'|
| 2 - 126 | reserved | Reserved for future use |
| 127* | Error | The sensor specified in 'Sensor Type' and 'Sensor Number' encountered an error |
### External Controller -> Webots Controller
|  Number             |      Name                    | Description|
|----------------|----------|-----------------------|
|128*| enableSensor | Enables the sensor specified in 'Sensor Type' and 'Sensor Number' |
|129*| disableSensor|  Disables the sensor specified in 'Sensor Type' and 'Sensor Number' |
|130| setValue | Changes a single configuration value the sensor specified in 'Sensor Type' and 'Sensor Number' |
|131*| setSensor | Sets all configuration values for the sensor specified in 'Sensor Type' and 'Sensor Number' |
|132 - 255 | reserved | Resered for future use |
Features marked with * are currently not implemented




<i>Sensor Type</i>:
We use the same Sensors Type definitions as Webots does.
In the enum: <b>WbNodeType</b> (include/controller/c/webots/nodes.h)
 Especially:


<ul>
	<li><b>37</b>: WB_NODE_DISTANCE_SENSOR</li>
	<li><b>46</b>: WB_NODE_LINEAR_MOTOR</li>
	<li><b>53</b>: WB_NODE_ROTATIONAL_MOTOR</li>
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
		<td><i>Distance Sensor</i></td>
		<td>8 byte <i>(Double)</i></td>
		<td>Value from  <i>wb_distance_sensor_get_value() </i></td>
		<td><a href="https://cyberbotics.com/doc/reference/distancesensor#wb_distance_sensor_get_value">here</a></td>
	</tr>
		<tr>
		<td><i>Linear Motor</i></td>
		<td>8 byte <i>(Double)</i></td>
		<td>Position value </i></td>
		<td><a href="https://cyberbotics.com/doc/reference/linearmotor">here</a></td>
	</tr>
		<tr>
		<td><i>Rotational Motor</i></td>
		<td>8 byte <i>(Double)</i></td>
		<td>Velocity value </i></td>
		<td><a href="https://cyberbotics.com/doc/reference/rotationalmotor">here</a></td>
	</tr>
	</table>
## Sensor ID
Again we use what Webots already provides: ID assignment. 
We use the Index Webots assigned to a tag as sensor id:

tag = wb_robot_get_device_by_index(sensor_id);

This way the aproach is completly scalable!

<h3>Usage</h3>
<ul>
	<li>Read header (first three byte)</li>
	<li>Evaluate sensor type and ID</li>
	<li>Get package body using the specified package body size</li>
</ul>

We created a Video explaning the Protocol. <a href="https://tubcloud.tu-berlin.de/s/GicfSHsYgn4D83Q">here</a> Password cab1wc2ec
