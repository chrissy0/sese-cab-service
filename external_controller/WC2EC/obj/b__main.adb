pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__main.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__main.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E074 : Short_Integer; pragma Import (Ada, E074, "system__os_lib_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exception_table_E");
   E042 : Short_Integer; pragma Import (Ada, E042, "ada__containers_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "ada__io_exceptions_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "ada__strings_E");
   E056 : Short_Integer; pragma Import (Ada, E056, "ada__strings__maps_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "ada__strings__maps__constants_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "interfaces__c_E");
   E029 : Short_Integer; pragma Import (Ada, E029, "system__exceptions_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "system__object_reader_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "system__dwarf_lines_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__soft_links__initialize_E");
   E041 : Short_Integer; pragma Import (Ada, E041, "system__traceback__symbolic_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "ada__tags_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "ada__streams_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "gnat_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "interfaces__c__strings_E");
   E128 : Short_Integer; pragma Import (Ada, E128, "system__file_control_block_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "system__finalization_root_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "ada__finalization_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "system__file_io_E");
   E156 : Short_Integer; pragma Import (Ada, E156, "system__storage_pools_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "system__finalization_masters_E");
   E227 : Short_Integer; pragma Import (Ada, E227, "system__storage_pools__subpools_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "system__task_info_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__calendar_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__calendar__delays_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "ada__real_time_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "ada__text_io_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "system__pool_global_E");
   E158 : Short_Integer; pragma Import (Ada, E158, "system__pool_size_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "gnat__sockets_E");
   E148 : Short_Integer; pragma Import (Ada, E148, "gnat__sockets__thin_common_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "gnat__sockets__thin_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "system__tasking__initialization_E");
   E205 : Short_Integer; pragma Import (Ada, E205, "system__tasking__protected_objects_E");
   E207 : Short_Integer; pragma Import (Ada, E207, "system__tasking__protected_objects__entries_E");
   E203 : Short_Integer; pragma Import (Ada, E203, "system__tasking__queuing_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "system__tasking__stages_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "distance_sensor_p_E");
   E221 : Short_Integer; pragma Import (Ada, E221, "ring_buffer_E");
   E219 : Short_Integer; pragma Import (Ada, E219, "wc2ec_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "wc2ec__finalize_body");
      begin
         E219 := E219 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "wc2ec__finalize_spec");
      begin
         F2;
      end;
      E207 := E207 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "gnat__sockets__finalize_body");
      begin
         E141 := E141 - 1;
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "gnat__sockets__finalize_spec");
      begin
         F5;
      end;
      E158 := E158 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "system__pool_size__finalize_spec");
      begin
         F6;
      end;
      E152 := E152 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__pool_global__finalize_spec");
      begin
         F7;
      end;
      E120 := E120 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "ada__text_io__finalize_spec");
      begin
         F8;
      end;
      E227 := E227 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__storage_pools__subpools__finalize_spec");
      begin
         F9;
      end;
      E164 := E164 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__finalization_masters__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__file_io__finalize_body");
      begin
         E124 := E124 - 1;
         F11;
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
          (True, True, False, False, True, True, True, False, 
           False, False, False, True, True, True, True, False, 
           False, False, False, False, True, True, False, True, 
           True, False, True, True, True, True, False, False, 
           False, False, False, True, False, True, True, False, 
           True, True, True, True, False, True, False, True, 
           False, False, True, True, False, True, True, False, 
           False, False, False, True, False, True, True, True, 
           False, False, True, False, True, True, True, False, 
           True, True, False, True, True, True, True, False, 
           False, True, False, False, False, True, False, False, 
           True, False, True, False),
         Count => (0, 0, 0, 1, 0, 0, 1, 0, 1, 0),
         Unknown => (False, False, False, False, False, False, True, False, True, False));
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
      E027 := E027 + 1;
      Ada.Containers'Elab_Spec;
      E042 := E042 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E070 := E070 + 1;
      Ada.Strings'Elab_Spec;
      E054 := E054 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E056 := E056 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E060 := E060 + 1;
      Interfaces.C'Elab_Spec;
      E080 := E080 + 1;
      System.Exceptions'Elab_Spec;
      E029 := E029 + 1;
      System.Object_Reader'Elab_Spec;
      E082 := E082 + 1;
      System.Dwarf_Lines'Elab_Spec;
      E049 := E049 + 1;
      System.Os_Lib'Elab_Body;
      E074 := E074 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E023 := E023 + 1;
      E015 := E015 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E041 := E041 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E108 := E108 + 1;
      Ada.Streams'Elab_Spec;
      E106 := E106 + 1;
      Gnat'Elab_Spec;
      E139 := E139 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E146 := E146 + 1;
      System.File_Control_Block'Elab_Spec;
      E128 := E128 + 1;
      System.Finalization_Root'Elab_Spec;
      E127 := E127 + 1;
      Ada.Finalization'Elab_Spec;
      E125 := E125 + 1;
      System.File_Io'Elab_Body;
      E124 := E124 + 1;
      System.Storage_Pools'Elab_Spec;
      E156 := E156 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E164 := E164 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E227 := E227 + 1;
      System.Task_Info'Elab_Spec;
      E181 := E181 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E008 := E008 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E006 := E006 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E217 := E217 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E120 := E120 + 1;
      System.Pool_Global'Elab_Spec;
      E152 := E152 + 1;
      System.Pool_Size'Elab_Spec;
      E158 := E158 + 1;
      Gnat.Sockets'Elab_Spec;
      E148 := E148 + 1;
      Gnat.Sockets.Thin'Elab_Body;
      E144 := E144 + 1;
      Gnat.Sockets'Elab_Body;
      E141 := E141 + 1;
      System.Tasking.Initialization'Elab_Body;
      E195 := E195 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E205 := E205 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E207 := E207 + 1;
      System.Tasking.Queuing'Elab_Body;
      E203 := E203 + 1;
      System.Tasking.Stages'Elab_Body;
      E191 := E191 + 1;
      E130 := E130 + 1;
      E221 := E221 + 1;
      WC2EC'ELAB_SPEC;
      WC2EC'ELAB_BODY;
      E219 := E219 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_main");

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
   --   C:\Users\mweis\Documents\git\cab_service\external_controller\WC2EC\obj\distance_sensor_p.o
   --   C:\Users\mweis\Documents\git\cab_service\external_controller\Ring_Buffer\obj\ring_buffer.o
   --   C:\Users\mweis\Documents\git\cab_service\external_controller\WC2EC\obj\wc2ec.o
   --   C:\Users\mweis\Documents\git\cab_service\external_controller\WC2EC\obj\main.o
   --   -LC:\Users\mweis\Documents\git\cab_service\external_controller\WC2EC\obj\
   --   -LC:\Users\mweis\Documents\git\cab_service\external_controller\WC2EC\obj\
   --   -LC:\Users\mweis\Documents\git\cab_service\external_controller\Ring_Buffer\obj\
   --   -LC:\gnat\2019\lib\aunit\
   --   -LC:/gnat/2019/lib/gcc/x86_64-pc-mingw32/8.3.1/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -lws2_32
   --   -Xlinker
   --   --stack=0x200000,0x1000
   --   -mthreads
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;
