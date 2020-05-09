#include <webots/LightSensor.hpp>
#include <webots/DistanceSensor.hpp>
#include <webots/Motor.hpp>
#include <webots/Robot.hpp>
#include <webots/camera.hpp>
#include <iostream>

#define TIME_STEP 64

using namespace webots;
using namespace std;

DistanceSensor *inf_l;
DistanceSensor *inf_c;
DistanceSensor *inf_r;
DistanceSensor *dist_l;
DistanceSensor *dist_c;
DistanceSensor *dist_r;
Camera *cam;
Motor *wheel_fl;
Motor *wheel_fr;
Motor *wheel_bl;
Motor *wheel_br;

void init(Robot *robot);
void followLine(double speed);

int main(int argc, char ** argv) {
  Robot * robot = new Robot();

  init(robot);

  while (robot->step(TIME_STEP) != -1) {
    // std::cout << inf_l->getValue() << " inf_l" << std::endl;
    // std::cout << inf_c->getValue() << " inf_c" << std::endl;
    // std::cout << inf_r->getValue() << " inf_r" << std::endl;
    std::cout << dist_l->getValue() << " dist_l" << std::endl;
    std::cout << dist_c->getValue() << " dist_c" << std::endl;
    std::cout << dist_r->getValue() << " dist_r" << std::endl;

    followLine(6.0);
  }

  delete robot;
  return 0;
}

void init(Robot * robot) {

  inf_l = robot->getDistanceSensor("inf_left");
  inf_l->enable(TIME_STEP);
  inf_c = robot->getDistanceSensor("inf_cent");
  inf_c->enable(TIME_STEP);
  inf_r = robot->getDistanceSensor("inf_right");
  inf_r->enable(TIME_STEP);
  dist_l = robot->getDistanceSensor("dist_l");
  dist_l->enable(TIME_STEP);
  dist_c = robot->getDistanceSensor("dist_c");
  dist_c->enable(TIME_STEP);
  dist_r = robot->getDistanceSensor("dist_r");
  dist_r->enable(TIME_STEP);
  cam = robot->getCamera("camera");
  cam->enable(TIME_STEP);

  wheel_fl = robot->getMotor("wheel1");
  wheel_fl->setPosition(INFINITY);
  wheel_fl->setVelocity(0.0);
  wheel_fr = robot->getMotor("wheel2");
  wheel_fr->setPosition(INFINITY);
  wheel_fr->setVelocity(0.0);
  wheel_bl = robot->getMotor("wheel3");
  wheel_bl->setPosition(INFINITY);
  wheel_bl->setVelocity(0.0);
  wheel_br = robot->getMotor("wheel4");
  wheel_br->setPosition(INFINITY);
  wheel_br->setVelocity(0.0);
}

void followLine(double speed) {
  double leftSpeed = speed;
  double rightSpeed = speed;

  if (dist_l->getValue() > 900 && dist_c->getValue() > 900 && dist_r->getValue() > 900) {
    if (inf_r->getValue() < 250 && inf_l->getValue() > 250) {
      // std::cout << "right" << std::endl;
      rightSpeed = 0;
    } else if (inf_l->getValue() < 250 && inf_r->getValue() > 250) {
      // std::cout << "left" << std::endl;
      leftSpeed = 0 * leftSpeed;
    }
  } else {
    leftSpeed = 0;
    rightSpeed = 0;
  }

  wheel_fl->setVelocity(leftSpeed);
  wheel_fr->setVelocity(rightSpeed);
  wheel_bl->setVelocity(leftSpeed);
  wheel_br->setVelocity(rightSpeed);
}
