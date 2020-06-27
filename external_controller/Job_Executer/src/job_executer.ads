with Motor_Controller; use Motor_Controller;
with Roadmarker; use Roadmarker;
package Job_Executer is
   
   type Intersection_Option_T is (Left, Right);
   

   task type Job_Executer_Task_T is
      entry Constructor
        (Motor_Controller_Task_A : in Motor_Controller_Task_Access_T;
         timeout_v               : in Duration;
         RM_get_sensor_value_a   : in get_roadmarker_sensor_value_access
        );
   end Job_Executer_Task_T;
   
   type Job_Executer_Task_Access_T is access Job_Executer_Task_T;

end Job_Executer;
