pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test_lane_detection.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test_lane_detection.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E075 : Short_Integer; pragma Import (Ada, E075, "system__os_lib_E");
   E018 : Short_Integer; pragma Import (Ada, E018, "system__soft_links_E");
   E028 : Short_Integer; pragma Import (Ada, E028, "system__exception_table_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "ada__containers_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "ada__io_exceptions_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__strings_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "ada__strings__maps_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "ada__strings__maps__constants_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "interfaces__c_E");
   E030 : Short_Integer; pragma Import (Ada, E030, "system__exceptions_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "system__object_reader_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "system__dwarf_lines_E");
   E024 : Short_Integer; pragma Import (Ada, E024, "system__soft_links__initialize_E");
   E042 : Short_Integer; pragma Import (Ada, E042, "system__traceback__symbolic_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "ada__tags_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "ada__streams_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "gnat_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "interfaces__c__strings_E");
   E223 : Short_Integer; pragma Import (Ada, E223, "system__file_control_block_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "system__finalization_root_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "ada__finalization_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "system__file_io_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "system__storage_pools_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "system__finalization_masters_E");
   E243 : Short_Integer; pragma Import (Ada, E243, "system__storage_pools__subpools_E");
   E186 : Short_Integer; pragma Import (Ada, E186, "system__task_info_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "ada__calendar_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "ada__calendar__delays_E");
   E237 : Short_Integer; pragma Import (Ada, E237, "ada__real_time_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "ada__text_io_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "system__pool_global_E");
   E202 : Short_Integer; pragma Import (Ada, E202, "system__tasking__initialization_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "system__tasking__protected_objects_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "system__tasking__protected_objects__entries_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "system__tasking__queuing_E");
   E235 : Short_Integer; pragma Import (Ada, E235, "system__tasking__stages_E");
   E239 : Short_Integer; pragma Import (Ada, E239, "system__tasking__async_delays_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "aunit_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "aunit__memory_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "aunit__memory__utils_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "ada_containers__aunit_lists_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "aunit__tests_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "aunit__time_measure_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "aunit__test_results_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "aunit__assertions_E");
   E111 : Short_Integer; pragma Import (Ada, E111, "aunit__test_filters_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "aunit__simple_test_cases_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "aunit__reporter_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "aunit__reporter__xml_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "aunit__test_fixtures_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "aunit__test_caller_E");
   E160 : Short_Integer; pragma Import (Ada, E160, "aunit__test_suites_E");
   E158 : Short_Integer; pragma Import (Ada, E158, "aunit__run_E");
   E227 : Short_Integer; pragma Import (Ada, E227, "roadmarker_functions_E");
   E225 : Short_Integer; pragma Import (Ada, E225, "lane_detection_E");
   E168 : Short_Integer; pragma Import (Ada, E168, "lane_detection_test_E");
   E162 : Short_Integer; pragma Import (Ada, E162, "lane_detection_suite_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "lane_detection_suite__finalize_body");
      begin
         E162 := E162 - 1;
         F1;
      end;
      E168 := E168 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "lane_detection_test__finalize_spec");
      begin
         F2;
      end;
      E160 := E160 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "aunit__test_suites__finalize_spec");
      begin
         F3;
      end;
      E166 := E166 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "aunit__test_fixtures__finalize_spec");
      begin
         F4;
      end;
      E152 := E152 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "aunit__reporter__xml__finalize_spec");
      begin
         F5;
      end;
      E111 := E111 - 1;
      E113 := E113 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "aunit__simple_test_cases__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "aunit__test_filters__finalize_spec");
      begin
         F7;
      end;
      E115 := E115 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "aunit__assertions__finalize_spec");
      begin
         F8;
      end;
      E122 := E122 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "aunit__test_results__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "aunit__tests__finalize_spec");
      begin
         E132 := E132 - 1;
         F10;
      end;
      E214 := E214 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F11;
      end;
      E143 := E143 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__pool_global__finalize_spec");
      begin
         F12;
      end;
      E218 := E218 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "ada__text_io__finalize_spec");
      begin
         F13;
      end;
      E243 := E243 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "system__storage_pools__subpools__finalize_spec");
      begin
         F14;
      end;
      E134 := E134 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "system__finalization_masters__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "system__file_io__finalize_body");
      begin
         E222 := E222 - 1;
         F16;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, True, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, False, False, True, True, False, False, 
           True, False, False, True, True, True, True, False, 
           False, False, False, False, True, True, False, True, 
           True, False, True, True, True, True, False, False, 
           False, False, False, True, False, False, True, False, 
           False, False, True, True, False, True, False, True, 
           True, False, False, True, False, False, False, False, 
           True, False, False, True, False, True, True, True, 
           False, False, True, False, True, True, True, False, 
           True, True, False, True, True, True, True, False, 
           False, True, False, False, False, False, False, True, 
           True, True, True, False),
         Count => (0, 0, 0, 0, 0, 1, 1, 1, 1, 0),
         Unknown => (False, False, False, False, False, False, False, True, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E028 := E028 + 1;
      Ada.Containers'Elab_Spec;
      E043 := E043 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E071 := E071 + 1;
      Ada.Strings'Elab_Spec;
      E055 := E055 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E057 := E057 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E061 := E061 + 1;
      Interfaces.C'Elab_Spec;
      E081 := E081 + 1;
      System.Exceptions'Elab_Spec;
      E030 := E030 + 1;
      System.Object_Reader'Elab_Spec;
      E083 := E083 + 1;
      System.Dwarf_Lines'Elab_Spec;
      E050 := E050 + 1;
      System.Os_Lib'Elab_Body;
      E075 := E075 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E024 := E024 + 1;
      E018 := E018 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E042 := E042 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E104 := E104 + 1;
      Ada.Streams'Elab_Spec;
      E102 := E102 + 1;
      Gnat'Elab_Spec;
      E146 := E146 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E176 := E176 + 1;
      System.File_Control_Block'Elab_Spec;
      E223 := E223 + 1;
      System.Finalization_Root'Elab_Spec;
      E139 := E139 + 1;
      Ada.Finalization'Elab_Spec;
      E137 := E137 + 1;
      System.File_Io'Elab_Body;
      E222 := E222 + 1;
      System.Storage_Pools'Elab_Spec;
      E141 := E141 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E134 := E134 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E243 := E243 + 1;
      System.Task_Info'Elab_Spec;
      E186 := E186 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E126 := E126 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E170 := E170 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E237 := E237 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E218 := E218 + 1;
      System.Pool_Global'Elab_Spec;
      E143 := E143 + 1;
      System.Tasking.Initialization'Elab_Body;
      E202 := E202 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E212 := E212 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E214 := E214 + 1;
      System.Tasking.Queuing'Elab_Body;
      E210 := E210 + 1;
      System.Tasking.Stages'Elab_Body;
      E235 := E235 + 1;
      System.Tasking.Async_Delays'Elab_Body;
      E239 := E239 + 1;
      E008 := E008 + 1;
      E005 := E005 + 1;
      E120 := E120 + 1;
      E117 := E117 + 1;
      Aunit.Tests'Elab_Spec;
      E132 := E132 + 1;
      Aunit.Time_Measure'Elab_Spec;
      E124 := E124 + 1;
      Aunit.Test_Results'Elab_Spec;
      E122 := E122 + 1;
      Aunit.Assertions'Elab_Spec;
      Aunit.Assertions'Elab_Body;
      E115 := E115 + 1;
      Aunit.Test_Filters'Elab_Spec;
      Aunit.Simple_Test_Cases'Elab_Spec;
      E113 := E113 + 1;
      E111 := E111 + 1;
      Aunit.Reporter'Elab_Spec;
      E012 := E012 + 1;
      Aunit.Reporter.Xml'Elab_Spec;
      E152 := E152 + 1;
      Aunit.Test_Fixtures'Elab_Spec;
      E166 := E166 + 1;
      E164 := E164 + 1;
      Aunit.Test_Suites'Elab_Spec;
      E160 := E160 + 1;
      E158 := E158 + 1;
      E227 := E227 + 1;
      Lane_Detection'Elab_Spec;
      Lane_Detection'Elab_Body;
      E225 := E225 + 1;
      Lane_Detection_Test'Elab_Spec;
      Lane_Detection_Test'Elab_Body;
      E168 := E168 + 1;
      Lane_Detection_Suite'Elab_Body;
      E162 := E162 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test_lane_detection");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   C:\Users\Dean_84\Desktop\Master\SS20\Project_SW of ES\Ada\Testing_Lane_Detection\Lane_Detection\obj\roadmarker_functions.o
   --   C:\Users\Dean_84\Desktop\Master\SS20\Project_SW of ES\Ada\Testing_Lane_Detection\Lane_Detection\obj\lane_detection.o
   --   C:\Users\Dean_84\Desktop\Master\SS20\Project_SW of ES\Ada\Testing_Lane_Detection\Lane_Detection_Test\obj\lane_detection_test.o
   --   C:\Users\Dean_84\Desktop\Master\SS20\Project_SW of ES\Ada\Testing_Lane_Detection\Lane_Detection_Test\obj\lane_detection_suite.o
   --   C:\Users\Dean_84\Desktop\Master\SS20\Project_SW of ES\Ada\Testing_Lane_Detection\Lane_Detection_Test\obj\test_lane_detection.o
   --   -LC:\Users\Dean_84\Desktop\Master\SS20\Project_SW of ES\Ada\Testing_Lane_Detection\Lane_Detection_Test\obj\
   --   -LC:\Users\Dean_84\Desktop\Master\SS20\Project_SW of ES\Ada\Testing_Lane_Detection\Lane_Detection_Test\obj\
   --   -LC:\Users\Dean_84\Desktop\Master\SS20\Project_SW of ES\Ada\Testing_Lane_Detection\Lane_Detection\obj\
   --   -LC:\gnat\2019\lib\aunit\
   --   -LC:/gnat/2019/lib/gcc/x86_64-pc-mingw32/8.3.1/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -Xlinker
   --   --stack=0x200000,0x1000
   --   -mthreads
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;
