with Motor_Controller; use Motor_Controller;
package Job_Executer is
   -- 0-15 Road Marker IDs
   -- 16   System Error
   -- 17   No Road Marker
   type Road_Marker_Done_T is new Integer range 0 .. 17;
   
   type Road_Marker_Next_T is (EMPTY_S, SHUTDOWN_S);
   
   type Intersection_Option_T is (Left, Right);
   

   task type Job_Executer_Task_T is
      entry Constructor
        (Motor_Controller_Task_A : in Motor_Controller_Task_Access_T;
         timeout_v               : in Duration
        );
      entry road_marker_done (Signal : in Road_Marker_Done_T);
      entry road_marker_next (Signal : out Road_Marker_Next_T);
   end Job_Executer_Task_T;

end Job_Executer;
