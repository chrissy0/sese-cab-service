#include <webots/LightSensor.hpp>
#include <webots/DistanceSensor.hpp>
#include <webots/Motor.hpp>
#include <webots/Robot.hpp>
#include <webots/camera.hpp>

#define TIME_STEP 64
using namespace webots;
using namespace std;
DistanceSensor *ds[5];
LightSensor *ls;
Camera  *cam[1];
Motor *wheels[4];
  
  char lsName[1][10] = {"light"};
  char dsNames[5][10] = {"ds_right", "ds_left", "inf_right","inf_cent","inf_left"};
  char camName[1][10] = {"camera"};
  char wheels_names[4][8] = {"wheel1", "wheel2", "wheel3", "wheel4"};

void Init(Robot *robot);
void Line_Algo(double leftSpeed,double rightSpeed);


int main(int argc, char **argv) {
  Robot *robot = new Robot();
  Init(robot);
 
   while (robot->step(TIME_STEP) != -1) {
         double leftSpeed = 3.0;
         double rightSpeed = 3.0;
          
         Line_Algo(leftSpeed,rightSpeed);
         const CameraRecognitionObject *objects =cam[0]->getRecognitionObjects();
         int number_of_objects = cam[0]->getRecognitionNumberOfObjects();
   }
  delete robot;
  return 0;  // EXIT_SUCCESS
}

void Init(Robot *robot)// Initialization
{
  ls = robot->getLightSensor(lsName[0]);
  ls->enable(TIME_STEP);
  cam[0] = robot->getCamera(camName[0]);
  cam[0]->enable(TIME_STEP);
  cam[0]->recognitionEnable(TIME_STEP);
  for (int i = 0; i < 5; i++) {
    ds[i] = robot->getDistanceSensor(dsNames[i]);
    ds[i]->enable(TIME_STEP);
  }
  
  for (int i = 0; i < 4; i++) {
    wheels[i] = robot->getMotor(wheels_names[i]);
    wheels[i]->setPosition(INFINITY);
    wheels[i]->setVelocity(0.0);
  }
}

void Line_Algo(double leftSpeed,double rightSpeed)// weß Linie Folgen
{
  if(ds[2]->getValue()<350 && ds[3]->getValue()>350 && ds[4]->getValue()>350){////// right_line
     rightSpeed= rightSpeed*0.5;
  } 
  else if(ds[2]->getValue()>350 && ds[3]->getValue()<350 && ds[4]->getValue()>350){////// center_line
  
  
  }
  else if(ds[2]->getValue()>350 && ds[3]->getValue()>350 && ds[4]->getValue()<350){////// left_line
  
    leftSpeed= leftSpeed*0.5;
  }
  else{
   
  }
    wheels[0]->setVelocity(leftSpeed);
    wheels[1]->setVelocity(rightSpeed);
    wheels[2]->setVelocity(leftSpeed);
    wheels[3]->setVelocity(rightSpeed);
}