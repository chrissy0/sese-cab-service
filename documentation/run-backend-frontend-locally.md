# Run Backend & Frontend locally
## Setup
- Install `JDK 8` - `adopt-openj9-1.8` was used during development
- Install `npm`
## Compilation + Execution
### Backend
- Navigate to the `backend` folder
- Inside a linux shell, run `./mvnw clean install -DskipTests`, or alternatively use `mvnw.cmd` on windows
- Start the backend via `java -jar target/cabservice-1.0.0.jar`
- The backend will be accessible at `localhost:8081`
### Frontend
- Navigate to the `frontend` folder
- Inside a linux shell, run `npm install` - or the windows equivalent
- Run `ng serve`
- The frontend will be accessible at `localhost:4200`
## Additional notes
To run everything locally, make sure the backend IP inside the external controller is set to `localhost`
