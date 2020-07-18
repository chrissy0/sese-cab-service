pragma Ada_2012; --updated
with Motor_Controller; use Motor_Controller;


package Motor_Controller_SPARK with SPARK_Mode is

   state_V          : Cab_State_T;
   motor_values_V   : Motor_Values_T;
   MOTOR_DRIVE_SPEED  : constant Long_Float := 3.0;
   MOTOR_ROTATE_SPEED : constant Long_Float := 1.0;
   JE_Next_Signal_V : Job_Executer_Next_t;
   LD_Next_Signal_V : Lane_Detection_Next_T;
   FD_Next_Signal_V : Front_Distance_Next_t;
   
    
    
   procedure SPARK_1 with
     Post => (if state_V.Driving = STRAIGHT then (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = MOTOR_DRIVE_SPEED)) else
             (if state_V.Driving = ROTATE_LEFT then (for all I in Vertical_Position_T'Range => motor_values_V( I, LEFT) = -MOTOR_DRIVE_SPEED and motor_values_V( I, RIGHT) = MOTOR_DRIVE_SPEED) else
             (if state_V.Driving = ROTATE_RIGHT then (for all I in Vertical_Position_T'Range => motor_values_V( I, LEFT) = MOTOR_DRIVE_SPEED and motor_values_V( I, RIGHT) = -MOTOR_DRIVE_SPEED) else
             (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0)))));
                        
   procedure SPARK_2 with
     Post => ( if state_V.Front_Is_Clear = DRIVE then  ( if state_V.Driving = STRAIGHT then (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = MOTOR_DRIVE_SPEED)) else
                                                       ( if state_V.Driving = ROTATE_LEFT then (for all I in Vertical_Position_T'Range => motor_values_V( I, LEFT) = -MOTOR_DRIVE_SPEED and motor_values_V( I, RIGHT) = MOTOR_DRIVE_SPEED) else
                                                       ( if state_V.Driving = ROTATE_RIGHT then (for all I in Vertical_Position_T'Range => motor_values_V( I, LEFT) = MOTOR_DRIVE_SPEED and motor_values_V( I, RIGHT) = -MOTOR_DRIVE_SPEED) else
                                                       ( for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0))))) 
             else
             ( if state_V.Front_Is_Clear = STOP  then (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0)) ));
       
       
   procedure SPARK_3 with
      Post => ( if state_V.No_System_Error = FRONT_CLEAR then ( JE_Next_Signal_V = EMPTY_S and (if state_V.Driving = STRAIGHT then (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = MOTOR_DRIVE_SPEED)) else
                                                                                               (if state_V.Driving = ROTATE_LEFT then (for all I in Vertical_Position_T'Range => motor_values_V( I, LEFT) = -MOTOR_DRIVE_SPEED and motor_values_V( I, RIGHT) = MOTOR_DRIVE_SPEED) else
                                                                                               (if state_V.Driving = ROTATE_RIGHT then (for all I in Vertical_Position_T'Range => motor_values_V( I, LEFT) = MOTOR_DRIVE_SPEED and motor_values_V( I, RIGHT) = -MOTOR_DRIVE_SPEED) else
                                                                                               (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0)))))) 
               else 
               ( if state_V.No_System_Error = FRONT_BLOCKED then   ( JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0)))));
                        
   procedure SPARK_4 with 
     Post => ( if state_V.Final_Safe_State = ROTATE_LEFT_90 then (JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => motor_values_V(I , LEFT) = 0.0 and motor_values_V(I , RIGHT) = MOTOR_ROTATE_SPEED *2.0)) else
             ( if state_V.Final_Safe_State = DRIVE_OFF_LEFT  then (JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = MOTOR_DRIVE_SPEED * 2.0))) else
             ( if state_V.Final_Safe_State = ROTATE_RIGHT_180_DEGREE then (JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => motor_values_V(I , LEFT) = MOTOR_ROTATE_SPEED*2.0 and  motor_values_V(I , RIGHT) = 0.0)) else
             ( if state_V.Final_Safe_State = DRIVE_OFF_RIGHT then (JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = MOTOR_DRIVE_SPEED * 2.0))) else
                  JE_Next_Signal_V = EMPTY_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0))))));
                             
                        
       
   procedure SPARK_5 with
     Post => ( if state_V.System_Error = FINAL_SAFE_STATE then ( if state_V.Final_Safe_State = ROTATE_LEFT_90 then (JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => motor_values_V(I , LEFT) = 0.0 and motor_values_V(I , RIGHT) = MOTOR_ROTATE_SPEED *2.0)) else
                                                               ( if state_V.Final_Safe_State = DRIVE_OFF_LEFT  then (JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = MOTOR_DRIVE_SPEED * 2.0))) else
                                                               ( if state_V.Final_Safe_State = ROTATE_RIGHT_180_DEGREE then (JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => motor_values_V(I , LEFT) = MOTOR_ROTATE_SPEED*2.0 and  motor_values_V(I , RIGHT) = 0.0)) else
                                                               ( if state_V.Final_Safe_State = DRIVE_OFF_RIGHT then (JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = MOTOR_DRIVE_SPEED * 2.0))) else
                                                                    JE_Next_Signal_V = EMPTY_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0)))))) else
                                                                    JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0)));
                        
   procedure SPARK_6 with
     Post =>( ( if state_V.Leaning = NEXT_LEFT  then LD_Next_Signal_V = LEAN_LEFT_S else
              ( if state_V.Leaning = NEXT_RIGHT then LD_Next_Signal_V = LEAN_RIGHT_S else
              ( if state_V.Leaning = LEAN_FROM_LINE then LD_Next_Signal_V = LEAN_FROM_LINE)))
              and
              ( if state_V.Base = SYSTEM_ERROR then FD_Next_Signal_V = EMPTY_S and ( if state_V.System_Error = FINAL_SAFE_STATE then ( if state_V.Final_Safe_State = ROTATE_LEFT_90 then (JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => motor_values_V(I , LEFT) = 0.0 and motor_values_V(I , RIGHT) = MOTOR_ROTATE_SPEED *2.0)) else
                                                                                                                                     ( if state_V.Final_Safe_State = DRIVE_OFF_LEFT  then (JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = MOTOR_DRIVE_SPEED * 2.0))) else
                                                                                                                                     ( if state_V.Final_Safe_State = ROTATE_RIGHT_180_DEGREE then (JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => motor_values_V(I , LEFT) = MOTOR_ROTATE_SPEED*2.0 and  motor_values_V(I , RIGHT) = 0.0)) else
                                                                                                                                     ( if state_V.Final_Safe_State = DRIVE_OFF_RIGHT then (JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = MOTOR_DRIVE_SPEED * 2.0))) else
                                                                                                                                       JE_Next_Signal_V = EMPTY_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0)))))) else
                                                                                     JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0)))                 else
              ( if state_V.Base = NO_SYSTEM_ERROR then FD_Next_Signal_V = EMPTY_S and ( if state_V.No_System_Error = FRONT_CLEAR then ( JE_Next_Signal_V = EMPTY_S and (if state_V.Driving = STRAIGHT then (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = MOTOR_DRIVE_SPEED)) else
                                                                                      ( if state_V.Driving = ROTATE_LEFT then (for all I in Vertical_Position_T'Range => motor_values_V( I, LEFT) = -MOTOR_DRIVE_SPEED and motor_values_V( I, RIGHT) = MOTOR_DRIVE_SPEED) else
                                                                                      ( if state_V.Driving = ROTATE_RIGHT then (for all I in Vertical_Position_T'Range => motor_values_V( I, LEFT) = MOTOR_DRIVE_SPEED and motor_values_V( I, RIGHT) = -MOTOR_DRIVE_SPEED) else
                                                                                      ( for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0)))))) 
                                                                                      else 
                                                                                      ( if state_V.No_System_Error = FRONT_BLOCKED then   ( JE_Next_Signal_V = BLOCKED_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0)))))         else
               LD_Next_Signal_V = SHUTDOWN_S and FD_Next_Signal_V = SHUTDOWN_S and JE_Next_Signal_V = SHUTDOWN_S and (for all I in Vertical_Position_T'Range => (for all J in Horizontal_Position_T'Range =>  motor_values_V(I,J) = 0.0)) ))) ;
end Motor_Controller_SPARK;
