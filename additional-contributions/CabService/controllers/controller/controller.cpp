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
DistanceSensor *inf_lb;
DistanceSensor *inf_cb;
DistanceSensor *inf_rb;

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

//////////////////////////////////////////////////not used////////////////////
Motor *slider_curb_lf;//not used
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
DistanceSensor *dist_wall_l;
DistanceSensor *dist_wall_r;
//////////////////////////////////////////////////////

void init(Robot *robot);
void followLine(double speed);
bool isActive(DistanceSensor *inf);
bool onRoadMarker();
bool rmFirstBit();
bool rmSecondBit();
bool rmThirdBit();
bool rmFourthBit();
int getRoadMarkerId();

void Depot_in();
void Depot_out();
void Obstacle_Avoidance();
void Inside_Path(int MarkerId);
void Order(int case_num);

int Obstacle_mode=0;
int MarkerId=0;
int StationPath_mode=0;
int Depot_Mode=-1;
int Out_cnt=0;

int Station_cnt =0;
int case_num =1;

int main(int argc, char ** argv) {
  Robot * robot = new Robot();

  init(robot);
 

  while (robot->step(TIME_STEP) != -1) {
     
     Order(case_num);
      
    
    if (dist_c->getValue() < 700)  Obstacle_mode++;
    
    if(onRoadMarker()){     
      MarkerId= getRoadMarkerId();
      if(MarkerId == 9 && Station_cnt==2) Depot_Mode=1; // activation of Depot
  //  else if(MarkerId == 7) Depot_Mode=-1;
      
      ////after the changing the route, chang back to the route before
      if((MarkerId !=2) && (MarkerId !=4) && (MarkerId !=6) && (MarkerId !=8)  && (Obstacle_mode==-1)){
        if(StationPath_mode ==0 ) StationPath_mode= 1;
        else StationPath_mode= 0;  
        
        Obstacle_mode++;    
      }
     }
     
   //////////////////////////// Depot_mode on /////////////////////////////       
     if (Depot_Mode ==1){
         Depot_in();         
     } 
     
     else if( Depot_Mode == -1){
         Depot_out();
          if(MarkerId == 1){ Depot_Mode =0;Out_cnt=0;}
     }
     
     else if(Obstacle_mode> 50){
         Obstacle_Avoidance();
      }
     
     ///////////////////////////on the Road/////////////////////
     else{
       
        if(StationPath_mode==1){
          Inside_Path(MarkerId);
          }
         else if(MarkerId ==9){ //pass the corner 9
            if(curb_rc->getValue()>500 && curb_rf->getValue()>800){
         
                wheel_fl->setVelocity(1);
                wheel_fr->setVelocity(0);
                wheel_bl->setVelocity(1);
                wheel_br->setVelocity(0);    
          
             }
             else followLine(2.0); 
         }
        else   followLine(7.0);  
          
     }
     
  }

  delete robot;
  return 0;
}
void Order(int case_num){
     
     switch(case_num)
     {
     case 1 :   //10 -> 11   In : 8 ,2  out :4
       if( MarkerId ==8 || MarkerId ==2){  StationPath_mode=1; }
       if( MarkerId ==4) StationPath_mode=0;
       if( Station_cnt ==2) Depot_Mode =1;
       cout<<"StationPath_mode : " << StationPath_mode<<endl;
         break;
    
   /*  case 2 :  //10 -> 12    In : 8 ,4  out : 2 , 6
     
         break;
         
     case 3 :  //10 -> 13   In : 8 ,6  out : 2 , 8
     
         break;
         
     case 4 :  //11 -> 10   In : 2 ,8  out : 4 , 2
     
         break;
         
     case 5 :  //11 -> 12   In : 2 ,4  out : 6
     
         break;
         
     case 6 :  //11 -> 13   In : 2 ,6  out : 4 , 8
     
         break;
     
     case 7 :  //12 -> 10   In : 4 ,8  out : 6 , 2
     
         break;
         
     case 8 :  //12 -> 11   In : 4 ,2  out : 6 ,4 //ausnahme
     
         break;
         
     case 9 :  //12 -> 13   In : 4 ,6  out : 8
     
         break;
         
     case 10 :  //13 -> 10   In : 6 ,8  out : 2
     
         break;
         
     case 11 : //13 -> 11   In : 6 ,2  out : 8 , 4
     
         break; 
         
     case 12 :  //13 -> 12   In : 6 ,4  out : 8 , 6
     
         break;
    
     */
       }
     } 


void Depot_in() {
         if(MarkerId == 0){
            if(curb_rc->getValue()>600 && curb_rf->getValue()>850){
         
                wheel_fl->setVelocity(1);
                wheel_fr->setVelocity(0);
                wheel_bl->setVelocity(1);
                wheel_br->setVelocity(0);              
             }              
             else followLine(2.0);         
         }   
                       
         else if(curb_lc->getValue()>650 && curb_lf->getValue()>800){                   
               wheel_fl->setVelocity(0);
                wheel_fr->setVelocity(2);
                wheel_bl->setVelocity(0);
                wheel_br->setVelocity(2);             
         }
         
         else if(MarkerId ==7){
             wheel_fl->setVelocity(0);
                wheel_fr->setVelocity(0);
                wheel_bl->setVelocity(0);
                wheel_br->setVelocity(0);   
         }         
         else followLine(1.0);

}

void Depot_out() {
  
    if(MarkerId ==7){
            Out_cnt++;            
             if(curb_lf->getValue()>800 && Out_cnt>30){
               
                wheel_fl->setVelocity(0);
                wheel_fr->setVelocity(1);
                wheel_bl->setVelocity(0);
                wheel_br->setVelocity(1);              
             }              
             else{ followLine(8.0);   }
         }
      else followLine(5.0);
       
}
//

void Obstacle_Avoidance(){
   double leftSpeed = -2;
  double rightSpeed = -2;
  int Temp_MarkerId=0;
  if( onRoadMarker() == true){ 
    if(getRoadMarkerId() ==2 || getRoadMarkerId() ==4 || getRoadMarkerId() ==6 ||getRoadMarkerId() ==8){
    
      if(Temp_MarkerId <10) StationPath_mode=1;
      else StationPath_mode =0;
    
    Obstacle_mode=-1;
    }
    else Temp_MarkerId= MarkerId;
  }
    
    if (inf_rb->getValue() < 250 && inf_lb->getValue() > 250) {//right
       
        rightSpeed = 0;
      } 
     else if (inf_lb->getValue() < 250 && inf_rb->getValue() > 250) { //left
      
        leftSpeed = 0 ;
     }
     
   wheel_fl->setVelocity(leftSpeed);
    wheel_fr->setVelocity(rightSpeed);
   wheel_bl->setVelocity(leftSpeed);
   wheel_br->setVelocity(rightSpeed);
  
 }


void Inside_Path( int MarkerId){
  if( MarkerId== 2 || MarkerId== 4 || MarkerId== 6 || MarkerId== 8){
       
      if(curb_rc->getValue()>850 && curb_rf->getValue()>850){
        
          
               wheel_fl->setVelocity(3);
                wheel_fr->setVelocity(0);
                wheel_bl->setVelocity(3);
                wheel_br->setVelocity(0);    
          
         }
         else followLine(5.0);
         
    }
    else followLine(7.0);
  
}


void followLine(double speed) {
  double leftSpeed = speed;
  double rightSpeed = speed;

  if (dist_l->getValue() > 350 && dist_c->getValue() > 700 && dist_r->getValue() > 350) {
    if (inf_r->getValue() < 250 && inf_l->getValue() > 250) {
     
      rightSpeed = 0;
    } else if (inf_l->getValue() < 250 && inf_r->getValue() > 250) {
     
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


void init(Robot *robot) {

  inf_l = robot->getDistanceSensor("inf_left");
  inf_l->enable(TIME_STEP);
  inf_c = robot->getDistanceSensor("inf_cent");
  inf_c->enable(TIME_STEP);
  inf_r = robot->getDistanceSensor("inf_right");
  inf_r->enable(TIME_STEP);
  inf_lb = robot->getDistanceSensor("inf_left_back");
  inf_lb->enable(TIME_STEP);
  inf_cb = robot->getDistanceSensor("inf_cent_back");
  inf_cb->enable(TIME_STEP);
  inf_rb = robot->getDistanceSensor("inf_right_back");
  inf_rb->enable(TIME_STEP);
  
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
  dist_wall_r = robot->getDistanceSensor("dist_wall_detector_right");
  dist_wall_r->enable(TIME_STEP);
  dist_wall_l = robot->getDistanceSensor("dist_wall_detector_left");
  dist_wall_l->enable(TIME_STEP);
 
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
}