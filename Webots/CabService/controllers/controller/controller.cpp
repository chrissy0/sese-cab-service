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
DistanceSensor *inf_rm_fr;
DistanceSensor *inf_rm_fl;
DistanceSensor *inf_rm_br;
DistanceSensor *inf_rm_bl;
DistanceSensor *inf_rm_fr_act;
DistanceSensor *inf_rm_fl_act;
DistanceSensor *inf_rm_br_act;
DistanceSensor *inf_rm_bl_act;
DistanceSensor *dist_l;
DistanceSensor *dist_c;
DistanceSensor *dist_r;
DistanceSensor *curb_rb; // curb distance sensor right back
DistanceSensor *curb_rc; // curb distance sensor right center
DistanceSensor *curb_rf; // curb distance sensor right front
DistanceSensor *curb_lf; // curb distance sensor left front
DistanceSensor *curb_lc; // curb distance sensor left center
DistanceSensor *curb_lb; // curb distance sensor left back
Motor *wheel_fl;
Motor *wheel_fr;
Motor *wheel_bl;
Motor *wheel_br;
Motor *slider_curb_lf;
Motor *slider_curb_lf2;
Motor *slider_curb_lc;
Motor *slider_curb_lc2;
Motor *slider_curb_lb;
Motor *slider_curb_lb2;
Motor *slider_curb_rf;
Motor *slider_curb_rf2;
Motor *slider_curb_rc;
Motor *slider_curb_rc2;
Motor *slider_curb_rb;
Motor *slider_curb_rb2;

void init(Robot *robot);
void followLine(double speed);
bool isActive(DistanceSensor *inf);
bool onRoadMarker();
bool rmFirstBit();
bool rmSecondBit();
bool rmThirdBit();
bool rmFourthBit();
int getRoadMarkerId();

int main(int argc, char ** argv) {
  Robot * robot = new Robot();

  init(robot);
  slider_curb_lf->setPosition(-0.01);
  slider_curb_lf2->setPosition(-0.01);
  slider_curb_lc->setPosition(-0.01);
  slider_curb_lc2->setPosition(-0.01);
  slider_curb_lb->setPosition(-0.01);
  slider_curb_lb2->setPosition(-0.01);
  slider_curb_rf->setPosition(-0.01);
  slider_curb_rf2->setPosition(-0.01);
  slider_curb_rc->setPosition(-0.01);
  slider_curb_rc2->setPosition(-0.01);
  slider_curb_rb->setPosition(-0.01);
  slider_curb_rb2->setPosition(-0.01);

  while (robot->step(TIME_STEP) != -1) {
    // std::cout << inf_l->getValue() << " inf_l" << std::endl;
    // std::cout << inf_c->getValue() << " inf_c" << std::endl;
    // std::cout << inf_r->getValue() << " inf_r" << std::endl;
    //std::cout << dist_l->getValue() << " dist_l" << std::endl;
    //std::cout << dist_c->getValue() << " dist_c" << std::endl;
    //std::cout << dist_r->getValue() << " dist_r" << std::endl;
    std::cout << curb_rb->getValue() << " curb_rb" << std::endl;
    std::cout << curb_rc->getValue() << " curb_rc" << std::endl;
    std::cout << curb_rf->getValue() << " curb_rf" << std::endl;
    std::cout << curb_lf->getValue() << " curb_lf" << std::endl;
    std::cout << curb_lc->getValue() << " curb_lc" << std::endl;
    std::cout << curb_lb->getValue() << " curb_lb" << std::endl;
    
    if (onRoadMarker()) {
      std::cout << getRoadMarkerId() << std::endl;
    }
    
    
    followLine(6.0);
  }

  delete robot;
  return 0;
}

void init(Robot *robot) {

  inf_l = robot->getDistanceSensor("inf_left");
  inf_l->enable(TIME_STEP);
  inf_c = robot->getDistanceSensor("inf_cent");
  inf_c->enable(TIME_STEP);
  inf_r = robot->getDistanceSensor("inf_right");
  inf_r->enable(TIME_STEP);
  inf_rm_fr = robot->getDistanceSensor("inf_rm_fr");
  inf_rm_fr->enable(TIME_STEP);
  inf_rm_fl = robot->getDistanceSensor("inf_rm_fl");
  inf_rm_fl->enable(TIME_STEP);
  inf_rm_br = robot->getDistanceSensor("inf_rm_br");
  inf_rm_br->enable(TIME_STEP);
  inf_rm_bl = robot->getDistanceSensor("inf_rm_bl");
  inf_rm_bl->enable(TIME_STEP);
  inf_rm_fr_act = robot->getDistanceSensor("inf_rm_fr_act");
  inf_rm_fr_act->enable(TIME_STEP);
  inf_rm_fl_act = robot->getDistanceSensor("inf_rm_fl_act");
  inf_rm_fl_act->enable(TIME_STEP);
  inf_rm_br_act = robot->getDistanceSensor("inf_rm_br_act");
  inf_rm_br_act->enable(TIME_STEP);
  inf_rm_bl_act = robot->getDistanceSensor("inf_rm_bl_act");
  inf_rm_bl_act->enable(TIME_STEP);  
  dist_l = robot->getDistanceSensor("dist_l");
  dist_l->enable(TIME_STEP);
  dist_c = robot->getDistanceSensor("dist_c");
  dist_c->enable(TIME_STEP);
  dist_r = robot->getDistanceSensor("dist_r");
  dist_r->enable(TIME_STEP);
  curb_rb = robot->getDistanceSensor("curb_rb");
  curb_rb->enable(TIME_STEP);
  curb_rc = robot->getDistanceSensor("curb_rc");
  curb_rc->enable(TIME_STEP);
  curb_rf = robot->getDistanceSensor("curb_rf");
  curb_rf->enable(TIME_STEP);
  curb_lf = robot->getDistanceSensor("curb_lf");
  curb_lf->enable(TIME_STEP);
  curb_lc = robot->getDistanceSensor("curb_lc");
  curb_lc->enable(TIME_STEP);
  curb_lb = robot->getDistanceSensor("curb_lb");
  curb_lb->enable(TIME_STEP);
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
  slider_curb_lf = robot->getMotor("slider_curb_lf");
  slider_curb_lf2 = robot->getMotor("slider_curb_lf2");
  slider_curb_lc = robot->getMotor("slider_curb_lc");
  slider_curb_lc2 = robot->getMotor("slider_curb_lc2");
  slider_curb_lb = robot->getMotor("slider_curb_lb");
  slider_curb_lb2 = robot->getMotor("slider_curb_lb2");
  slider_curb_rf = robot->getMotor("slider_curb_rf");
  slider_curb_rf2 = robot->getMotor("slider_curb_rf2");
  slider_curb_rc = robot->getMotor("slider_curb_rc");
  slider_curb_rc2 = robot->getMotor("slider_curb_rc2");
  slider_curb_rb = robot->getMotor("slider_curb_rb");
  slider_curb_rb2 = robot->getMotor("slider_curb_rb2");
}

void followLine(double speed) {
  double leftSpeed = speed;
  double rightSpeed = speed;

  if (dist_l->getValue() > 350 && dist_c->getValue() > 700 && dist_r->getValue() > 350) {
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

bool isActive(DistanceSensor *inf) {
  return inf->getValue() < 250;
}

bool onRoadMarker() {
  return isActive(inf_rm_fr_act) && 
    isActive(inf_rm_fl_act) && 
    isActive(inf_rm_br_act) && 
    isActive(inf_rm_bl_act);
}

bool rmFirstBit() {
  return isActive(inf_rm_fl);
}

bool rmSecondBit() {
  return isActive(inf_rm_fr);
}

bool rmThirdBit() {
  return isActive(inf_rm_bl);
}

bool rmFourthBit() {
  return isActive(inf_rm_br);
}

int getRoadMarkerId() {
  return rmFirstBit() + 2 * rmSecondBit() + 4 * rmThirdBit() + 8 * rmFourthBit();
}
