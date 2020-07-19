# Security Concept External Controller
In order to make the external controller more fault tolerant, each Sensor has at least one identical backup sensor. The front distance sensor, which is used to prevent collisions and thereby the most important sensor, is backed up by an additional array of US. This drastically increases the reliability of the collision prevention. The curb detection is a backup level to the line detection. When the line detection fails, the cab uses the curb detection to safely move the cab back to the sidetrack.

To visualize the different fall-back levels, we created the following fault tree diagrams:
`FTA_Final_Safe_State.pdf`: Fault Tree Diagram for when the Final Safe State is activated. IN this state, the cab tries to drive off the track. This will only work when the simulation is at full speed while in this tate
`FTA_Side_Track.pdf`: Fault Tree Diagram for when the Side Track State is activated. In this state, the cab navigates itself back to the side track to not block the road. The cab is unable to execute jobs in this state.
`FTA_Stay_on_Track.pdf`: Fault Tree Diagram for when the Side Track State is activated. In this state, the cab stops driving because it cannot ensure avoiding collisions.

Other files in this directory: 
`Code Documentation/ `: Documentation of the ADA code.
`External Controller Simple Data Flow Diagram.pdf`: Simple Data Flow Diagram used in the 3rd milestone presentation