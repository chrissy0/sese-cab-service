# Setup & Run Simulation
## Setup

### Backend & Frontend

- Install `JDK 8` - `adopt-openj9-1.8` was used during development

- Install `npm`

  #### Backend

  - Navigate to the `backend` folder
  - Inside a linux shell, run `./mvnw clean install -DskipTests`, or alternatively use `mvnw.cmd` on windows
  - Start the backend via `java -jar target/cabservice-1.0.0.jar`
  - The backend will be accessible at `localhost:8081`

  #### Frontend

  - Navigate to the `frontend` folder
  - Inside a linux shell, run `npm install` - or the windows equivalent
  - Run `ng serve`
  - The frontend will be accessible at `localhost:4200`

### Webots world

- Navigate to `Webots/CabService/worlds`
- Open the file `cab.wbt`
- In the webots program go to `File -> Open Text File`
- Navigate to `Webots/CabServie/controllers/WC2EC_Controller`
- In th upcoming window open the file `Webots/CabServie/controllers/WC2EC_Controller/WC2EC_Controller.c`
- Click on the right hand side on the gear wheel to build the Controller
### External Controller

- install `GNAT Studio`
- navigate to `external_controller/External_Controller`
- open `external_controller.gpr` with `GNAT Studio`
- in `GNAT Studio` go to ``Build -> Project -> Build_All``
- in the upcoming window click `Execute`
## Run
- In Webots press the play button
- In GNAT Studio press `Shift+F2`
- in the upcoming window insert `%E 127.0.0.1 27015 yellowCab 14`
- Press `Execute`
- Press `Shift+F2` again
- in the upcoming window insert `%E 127.0.0.1 27016 redCab 14`
- Press `Execute`
Press `Shift+F2` again
- in the upcoming window insert `%E 127.0.0.1 27017 greenCab 14`
- Press `Execute`
- Simulation in Webots is running
- Control jobs via Frontend

# Project Structure
```
sese-cab-service
├── Webots/CabService/
│          ├── controllers/WC2EC_Controller/  # Internal Controller C Files
│          └── worlds/cab.wbt                 # Webots world    
├── additional-contributions/
│   ├── statechart/                           # External Controller State chart model
│   │                                         # (deprecated)
│   └── webots/                               # First Webots prototypes (deprecated)
├── backend/                                  # Backend sourcefiles
├── documentation/
│   ├── Coding_Standards/                     # Coding standard specifications
│   ├── external_controller/                  # Documentation
│   │   ├── Code Documentation                # Generated ADA code documentation
│   │   └── Security Concept.md               # Security Concept Description 
│   ├── interface-definitions.md              # External Controller to backend interface
│   ├── QA.md                                 # Quality Assurance
│   ├── run-backend-frontend-locally.md       # Guide to locally run the backend
│   ├── sensors.md                            # Sensor description
│   ├── setup-jenkins.md                      # Guide to locally run the CI
│   ├── simulate-communication-failure.md
│   ├── verification.md                       # Guide to run the verification
│   ├── WC2EC.md                              # Webots controller to external controller
│                                             # interface
├── external_controller/                      # External controller ADA projects
│   └── DEPRECATED/                           # Deprecated ADA projects
└── frontend/
└── responsinilities.md                       # Project responsibilities
```