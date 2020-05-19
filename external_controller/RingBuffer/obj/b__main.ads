pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 8.4.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_main" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#b6daa40f#;
   pragma Export (C, u00001, "mainB");
   u00002 : constant Version_32 := 16#050ff2f0#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#4113f22b#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#b8041258#;
   pragma Export (C, u00005, "ada__calendar__delaysB");
   u00006 : constant Version_32 := 16#b27fb9e9#;
   pragma Export (C, u00006, "ada__calendar__delaysS");
   u00007 : constant Version_32 := 16#32ac7bbb#;
   pragma Export (C, u00007, "ada__calendarB");
   u00008 : constant Version_32 := 16#5b279c75#;
   pragma Export (C, u00008, "ada__calendarS");
   u00009 : constant Version_32 := 16#f52efeca#;
   pragma Export (C, u00009, "ada__exceptionsB");
   u00010 : constant Version_32 := 16#e824681c#;
   pragma Export (C, u00010, "ada__exceptionsS");
   u00011 : constant Version_32 := 16#5726abed#;
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerB");
   u00012 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00012, "ada__exceptions__last_chance_handlerS");
   u00013 : constant Version_32 := 16#4635ec04#;
   pragma Export (C, u00013, "systemS");
   u00014 : constant Version_32 := 16#ae860117#;
   pragma Export (C, u00014, "system__soft_linksB");
   u00015 : constant Version_32 := 16#bbca7dc7#;
   pragma Export (C, u00015, "system__soft_linksS");
   u00016 : constant Version_32 := 16#20d205ed#;
   pragma Export (C, u00016, "system__secondary_stackB");
   u00017 : constant Version_32 := 16#bb5d8e68#;
   pragma Export (C, u00017, "system__secondary_stackS");
   u00018 : constant Version_32 := 16#36a16434#;
   pragma Export (C, u00018, "system__parametersB");
   u00019 : constant Version_32 := 16#bea32858#;
   pragma Export (C, u00019, "system__parametersS");
   u00020 : constant Version_32 := 16#ced09590#;
   pragma Export (C, u00020, "system__storage_elementsB");
   u00021 : constant Version_32 := 16#6bf6a600#;
   pragma Export (C, u00021, "system__storage_elementsS");
   u00022 : constant Version_32 := 16#7d395b5e#;
   pragma Export (C, u00022, "system__soft_links__initializeB");
   u00023 : constant Version_32 := 16#5697fc2b#;
   pragma Export (C, u00023, "system__soft_links__initializeS");
   u00024 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00024, "system__stack_checkingB");
   u00025 : constant Version_32 := 16#c88a87ec#;
   pragma Export (C, u00025, "system__stack_checkingS");
   u00026 : constant Version_32 := 16#34742901#;
   pragma Export (C, u00026, "system__exception_tableB");
   u00027 : constant Version_32 := 16#1b9b8546#;
   pragma Export (C, u00027, "system__exception_tableS");
   u00028 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00028, "system__exceptionsB");
   u00029 : constant Version_32 := 16#2e5681f2#;
   pragma Export (C, u00029, "system__exceptionsS");
   u00030 : constant Version_32 := 16#80916427#;
   pragma Export (C, u00030, "system__exceptions__machineB");
   u00031 : constant Version_32 := 16#3bad9081#;
   pragma Export (C, u00031, "system__exceptions__machineS");
   u00032 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00032, "system__exceptions_debugB");
   u00033 : constant Version_32 := 16#38bf15c0#;
   pragma Export (C, u00033, "system__exceptions_debugS");
   u00034 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00034, "system__img_intB");
   u00035 : constant Version_32 := 16#44ee0cc6#;
   pragma Export (C, u00035, "system__img_intS");
   u00036 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00036, "system__tracebackB");
   u00037 : constant Version_32 := 16#181732c0#;
   pragma Export (C, u00037, "system__tracebackS");
   u00038 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00038, "system__traceback_entriesB");
   u00039 : constant Version_32 := 16#466e1a74#;
   pragma Export (C, u00039, "system__traceback_entriesS");
   u00040 : constant Version_32 := 16#c2486b24#;
   pragma Export (C, u00040, "system__traceback__symbolicB");
   u00041 : constant Version_32 := 16#c84061d1#;
   pragma Export (C, u00041, "system__traceback__symbolicS");
   u00042 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00042, "ada__containersS");
   u00043 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00043, "ada__exceptions__tracebackB");
   u00044 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00044, "ada__exceptions__tracebackS");
   u00045 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00045, "interfacesS");
   u00046 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00046, "interfaces__cB");
   u00047 : constant Version_32 := 16#f60287af#;
   pragma Export (C, u00047, "interfaces__cS");
   u00048 : constant Version_32 := 16#e865e681#;
   pragma Export (C, u00048, "system__bounded_stringsB");
   u00049 : constant Version_32 := 16#31c8cd1d#;
   pragma Export (C, u00049, "system__bounded_stringsS");
   u00050 : constant Version_32 := 16#b018f329#;
   pragma Export (C, u00050, "system__crtlS");
   u00051 : constant Version_32 := 16#2260731f#;
   pragma Export (C, u00051, "system__dwarf_linesB");
   u00052 : constant Version_32 := 16#5f137e60#;
   pragma Export (C, u00052, "system__dwarf_linesS");
   u00053 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00053, "ada__charactersS");
   u00054 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00054, "ada__characters__handlingB");
   u00055 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00055, "ada__characters__handlingS");
   u00056 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00056, "ada__characters__latin_1S");
   u00057 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00057, "ada__stringsS");
   u00058 : constant Version_32 := 16#96df1a3f#;
   pragma Export (C, u00058, "ada__strings__mapsB");
   u00059 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00059, "ada__strings__mapsS");
   u00060 : constant Version_32 := 16#d68fb8f1#;
   pragma Export (C, u00060, "system__bit_opsB");
   u00061 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00061, "system__bit_opsS");
   u00062 : constant Version_32 := 16#72b39087#;
   pragma Export (C, u00062, "system__unsigned_typesS");
   u00063 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00063, "ada__strings__maps__constantsS");
   u00064 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00064, "system__address_imageB");
   u00065 : constant Version_32 := 16#e7d9713e#;
   pragma Export (C, u00065, "system__address_imageS");
   u00066 : constant Version_32 := 16#ec78c2bf#;
   pragma Export (C, u00066, "system__img_unsB");
   u00067 : constant Version_32 := 16#ed47ac70#;
   pragma Export (C, u00067, "system__img_unsS");
   u00068 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00068, "system__ioB");
   u00069 : constant Version_32 := 16#d8771b4b#;
   pragma Export (C, u00069, "system__ioS");
   u00070 : constant Version_32 := 16#f790d1ef#;
   pragma Export (C, u00070, "system__mmapB");
   u00071 : constant Version_32 := 16#7c445363#;
   pragma Export (C, u00071, "system__mmapS");
   u00072 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00072, "ada__io_exceptionsS");
   u00073 : constant Version_32 := 16#0cdaa54a#;
   pragma Export (C, u00073, "system__mmap__os_interfaceB");
   u00074 : constant Version_32 := 16#82f29877#;
   pragma Export (C, u00074, "system__mmap__os_interfaceS");
   u00075 : constant Version_32 := 16#834dfe5e#;
   pragma Export (C, u00075, "system__mmap__unixS");
   u00076 : constant Version_32 := 16#68267aea#;
   pragma Export (C, u00076, "system__os_libB");
   u00077 : constant Version_32 := 16#4542b55d#;
   pragma Export (C, u00077, "system__os_libS");
   u00078 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00078, "system__case_utilB");
   u00079 : constant Version_32 := 16#623c85d3#;
   pragma Export (C, u00079, "system__case_utilS");
   u00080 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00080, "system__stringsB");
   u00081 : constant Version_32 := 16#2623c091#;
   pragma Export (C, u00081, "system__stringsS");
   u00082 : constant Version_32 := 16#ef6ff0b4#;
   pragma Export (C, u00082, "system__object_readerB");
   u00083 : constant Version_32 := 16#0b06497e#;
   pragma Export (C, u00083, "system__object_readerS");
   u00084 : constant Version_32 := 16#1a74a354#;
   pragma Export (C, u00084, "system__val_lliB");
   u00085 : constant Version_32 := 16#dc110aa4#;
   pragma Export (C, u00085, "system__val_lliS");
   u00086 : constant Version_32 := 16#afdbf393#;
   pragma Export (C, u00086, "system__val_lluB");
   u00087 : constant Version_32 := 16#0841c7f5#;
   pragma Export (C, u00087, "system__val_lluS");
   u00088 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00088, "system__val_utilB");
   u00089 : constant Version_32 := 16#ea955afa#;
   pragma Export (C, u00089, "system__val_utilS");
   u00090 : constant Version_32 := 16#d7bf3f29#;
   pragma Export (C, u00090, "system__exception_tracesB");
   u00091 : constant Version_32 := 16#62eacc9e#;
   pragma Export (C, u00091, "system__exception_tracesS");
   u00092 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00092, "system__wch_conB");
   u00093 : constant Version_32 := 16#5d48ced6#;
   pragma Export (C, u00093, "system__wch_conS");
   u00094 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00094, "system__wch_stwB");
   u00095 : constant Version_32 := 16#7059e2d7#;
   pragma Export (C, u00095, "system__wch_stwS");
   u00096 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00096, "system__wch_cnvB");
   u00097 : constant Version_32 := 16#52ff7425#;
   pragma Export (C, u00097, "system__wch_cnvS");
   u00098 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00098, "system__wch_jisB");
   u00099 : constant Version_32 := 16#d28f6d04#;
   pragma Export (C, u00099, "system__wch_jisS");
   u00100 : constant Version_32 := 16#d083f760#;
   pragma Export (C, u00100, "system__os_primitivesB");
   u00101 : constant Version_32 := 16#ccbafd72#;
   pragma Export (C, u00101, "system__os_primitivesS");
   u00102 : constant Version_32 := 16#9048bac9#;
   pragma Export (C, u00102, "ada__real_timeB");
   u00103 : constant Version_32 := 16#c3d451b0#;
   pragma Export (C, u00103, "ada__real_timeS");
   u00104 : constant Version_32 := 16#5f53885a#;
   pragma Export (C, u00104, "system__taskingB");
   u00105 : constant Version_32 := 16#c57f5c12#;
   pragma Export (C, u00105, "system__taskingS");
   u00106 : constant Version_32 := 16#74a74685#;
   pragma Export (C, u00106, "system__task_primitivesS");
   u00107 : constant Version_32 := 16#b541254b#;
   pragma Export (C, u00107, "system__os_interfaceB");
   u00108 : constant Version_32 := 16#df45d786#;
   pragma Export (C, u00108, "system__os_interfaceS");
   u00109 : constant Version_32 := 16#3dce974e#;
   pragma Export (C, u00109, "system__linuxS");
   u00110 : constant Version_32 := 16#389cf034#;
   pragma Export (C, u00110, "system__os_constantsS");
   u00111 : constant Version_32 := 16#77840e8c#;
   pragma Export (C, u00111, "system__task_primitives__operationsB");
   u00112 : constant Version_32 := 16#ca5a93a0#;
   pragma Export (C, u00112, "system__task_primitives__operationsS");
   u00113 : constant Version_32 := 16#71c5de81#;
   pragma Export (C, u00113, "system__interrupt_managementB");
   u00114 : constant Version_32 := 16#93368995#;
   pragma Export (C, u00114, "system__interrupt_managementS");
   u00115 : constant Version_32 := 16#f65595cf#;
   pragma Export (C, u00115, "system__multiprocessorsB");
   u00116 : constant Version_32 := 16#7e997377#;
   pragma Export (C, u00116, "system__multiprocessorsS");
   u00117 : constant Version_32 := 16#375a3ef7#;
   pragma Export (C, u00117, "system__task_infoB");
   u00118 : constant Version_32 := 16#ab92045a#;
   pragma Export (C, u00118, "system__task_infoS");
   u00119 : constant Version_32 := 16#8788dcdf#;
   pragma Export (C, u00119, "system__tasking__debugB");
   u00120 : constant Version_32 := 16#19310ffa#;
   pragma Export (C, u00120, "system__tasking__debugS");
   u00121 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00121, "system__concat_2B");
   u00122 : constant Version_32 := 16#44953bd4#;
   pragma Export (C, u00122, "system__concat_2S");
   u00123 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00123, "system__concat_3B");
   u00124 : constant Version_32 := 16#4d45b0a1#;
   pragma Export (C, u00124, "system__concat_3S");
   u00125 : constant Version_32 := 16#273384e4#;
   pragma Export (C, u00125, "system__img_enum_newB");
   u00126 : constant Version_32 := 16#2779eac4#;
   pragma Export (C, u00126, "system__img_enum_newS");
   u00127 : constant Version_32 := 16#9dca6636#;
   pragma Export (C, u00127, "system__img_lliB");
   u00128 : constant Version_32 := 16#577ab9d5#;
   pragma Export (C, u00128, "system__img_lliS");
   u00129 : constant Version_32 := 16#deb95810#;
   pragma Export (C, u00129, "system__stack_usageB");
   u00130 : constant Version_32 := 16#3a3ac346#;
   pragma Export (C, u00130, "system__stack_usageS");
   u00131 : constant Version_32 := 16#d398a95f#;
   pragma Export (C, u00131, "ada__tagsB");
   u00132 : constant Version_32 := 16#12a0afb8#;
   pragma Export (C, u00132, "ada__tagsS");
   u00133 : constant Version_32 := 16#796f31f1#;
   pragma Export (C, u00133, "system__htableB");
   u00134 : constant Version_32 := 16#c2f75fee#;
   pragma Export (C, u00134, "system__htableS");
   u00135 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00135, "system__string_hashB");
   u00136 : constant Version_32 := 16#60a93490#;
   pragma Export (C, u00136, "system__string_hashS");
   u00137 : constant Version_32 := 16#927a893f#;
   pragma Export (C, u00137, "ada__text_ioB");
   u00138 : constant Version_32 := 16#5194351e#;
   pragma Export (C, u00138, "ada__text_ioS");
   u00139 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00139, "ada__streamsB");
   u00140 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00140, "ada__streamsS");
   u00141 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00141, "interfaces__c_streamsB");
   u00142 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00142, "interfaces__c_streamsS");
   u00143 : constant Version_32 := 16#6a70d424#;
   pragma Export (C, u00143, "system__file_ioB");
   u00144 : constant Version_32 := 16#e1440d61#;
   pragma Export (C, u00144, "system__file_ioS");
   u00145 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00145, "ada__finalizationS");
   u00146 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00146, "system__finalization_rootB");
   u00147 : constant Version_32 := 16#09c79f94#;
   pragma Export (C, u00147, "system__finalization_rootS");
   u00148 : constant Version_32 := 16#bbaa76ac#;
   pragma Export (C, u00148, "system__file_control_blockS");
   u00149 : constant Version_32 := 16#79065845#;
   pragma Export (C, u00149, "rbB");
   u00150 : constant Version_32 := 16#0a17cb7e#;
   pragma Export (C, u00150, "rbS");
   u00151 : constant Version_32 := 16#d96e3c40#;
   pragma Export (C, u00151, "system__finalization_mastersB");
   u00152 : constant Version_32 := 16#1dc9d5ce#;
   pragma Export (C, u00152, "system__finalization_mastersS");
   u00153 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00153, "system__img_boolB");
   u00154 : constant Version_32 := 16#b3ec9def#;
   pragma Export (C, u00154, "system__img_boolS");
   u00155 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00155, "system__storage_poolsB");
   u00156 : constant Version_32 := 16#65d872a9#;
   pragma Export (C, u00156, "system__storage_poolsS");
   u00157 : constant Version_32 := 16#5a895de2#;
   pragma Export (C, u00157, "system__pool_globalB");
   u00158 : constant Version_32 := 16#7141203e#;
   pragma Export (C, u00158, "system__pool_globalS");
   u00159 : constant Version_32 := 16#935938d8#;
   pragma Export (C, u00159, "system__memoryB");
   u00160 : constant Version_32 := 16#1f488a30#;
   pragma Export (C, u00160, "system__memoryS");
   u00161 : constant Version_32 := 16#2e260032#;
   pragma Export (C, u00161, "system__storage_pools__subpoolsB");
   u00162 : constant Version_32 := 16#cc5a1856#;
   pragma Export (C, u00162, "system__storage_pools__subpoolsS");
   u00163 : constant Version_32 := 16#84042202#;
   pragma Export (C, u00163, "system__storage_pools__subpools__finalizationB");
   u00164 : constant Version_32 := 16#fe2f4b3a#;
   pragma Export (C, u00164, "system__storage_pools__subpools__finalizationS");
   u00165 : constant Version_32 := 16#00f77f90#;
   pragma Export (C, u00165, "system__tasking__protected_objectsB");
   u00166 : constant Version_32 := 16#b15a1586#;
   pragma Export (C, u00166, "system__tasking__protected_objectsS");
   u00167 : constant Version_32 := 16#00237e5e#;
   pragma Export (C, u00167, "system__soft_links__taskingB");
   u00168 : constant Version_32 := 16#e939497e#;
   pragma Export (C, u00168, "system__soft_links__taskingS");
   u00169 : constant Version_32 := 16#17d21067#;
   pragma Export (C, u00169, "ada__exceptions__is_null_occurrenceB");
   u00170 : constant Version_32 := 16#e1d7566f#;
   pragma Export (C, u00170, "ada__exceptions__is_null_occurrenceS");
   u00171 : constant Version_32 := 16#1af89ec4#;
   pragma Export (C, u00171, "system__tasking__protected_objects__entriesB");
   u00172 : constant Version_32 := 16#7daf93e7#;
   pragma Export (C, u00172, "system__tasking__protected_objects__entriesS");
   u00173 : constant Version_32 := 16#100eaf58#;
   pragma Export (C, u00173, "system__restrictionsB");
   u00174 : constant Version_32 := 16#0d473555#;
   pragma Export (C, u00174, "system__restrictionsS");
   u00175 : constant Version_32 := 16#25da1d82#;
   pragma Export (C, u00175, "system__tasking__initializationB");
   u00176 : constant Version_32 := 16#f7885a93#;
   pragma Export (C, u00176, "system__tasking__initializationS");
   u00177 : constant Version_32 := 16#ea260e8c#;
   pragma Export (C, u00177, "system__tasking__task_attributesB");
   u00178 : constant Version_32 := 16#4c40320c#;
   pragma Export (C, u00178, "system__tasking__task_attributesS");
   u00179 : constant Version_32 := 16#eb5dbcec#;
   pragma Export (C, u00179, "system__tasking__protected_objects__operationsB");
   u00180 : constant Version_32 := 16#ba36ad85#;
   pragma Export (C, u00180, "system__tasking__protected_objects__operationsS");
   u00181 : constant Version_32 := 16#0a70ebb8#;
   pragma Export (C, u00181, "system__tasking__entry_callsB");
   u00182 : constant Version_32 := 16#c7180c67#;
   pragma Export (C, u00182, "system__tasking__entry_callsS");
   u00183 : constant Version_32 := 16#a67d6c32#;
   pragma Export (C, u00183, "system__tasking__queuingB");
   u00184 : constant Version_32 := 16#c9e0262c#;
   pragma Export (C, u00184, "system__tasking__queuingS");
   u00185 : constant Version_32 := 16#3a943a7f#;
   pragma Export (C, u00185, "system__tasking__utilitiesB");
   u00186 : constant Version_32 := 16#ab3d060e#;
   pragma Export (C, u00186, "system__tasking__utilitiesS");
   u00187 : constant Version_32 := 16#458801a6#;
   pragma Export (C, u00187, "system__tasking__rendezvousB");
   u00188 : constant Version_32 := 16#f242aaf9#;
   pragma Export (C, u00188, "system__tasking__rendezvousS");
   u00189 : constant Version_32 := 16#90fc4e20#;
   pragma Export (C, u00189, "system__tasking__stagesB");
   u00190 : constant Version_32 := 16#aafe2bf6#;
   pragma Export (C, u00190, "system__tasking__stagesS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.restrictions%s
   --  system.restrictions%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  system.exception_traces%s
   --  ada.exceptions%s
   --  system.wch_stw%s
   --  system.val_util%s
   --  system.val_llu%s
   --  system.val_lli%s
   --  system.os_lib%s
   --  system.bit_ops%s
   --  ada.characters.handling%s
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.soft_links%s
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.containers%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.secondary_stack%b
   --  system.address_image%s
   --  system.bounded_strings%s
   --  ada.exceptions.last_chance_handler%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.exception_traces%b
   --  system.memory%s
   --  system.memory%b
   --  system.wch_stw%b
   --  system.val_util%b
   --  system.val_llu%b
   --  system.val_lli%b
   --  system.os_lib%b
   --  system.bit_ops%b
   --  ada.strings.maps%s
   --  ada.strings.maps.constants%s
   --  ada.characters.handling%b
   --  interfaces.c%s
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.address_image%b
   --  system.bounded_strings%b
   --  ada.exceptions.last_chance_handler%b
   --  system.standard_library%b
   --  system.mmap%s
   --  ada.strings.maps%b
   --  interfaces.c%b
   --  system.object_reader%s
   --  system.dwarf_lines%s
   --  system.dwarf_lines%b
   --  system.mmap.unix%s
   --  system.mmap.os_interface%s
   --  system.mmap%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  system.object_reader%b
   --  system.mmap.os_interface%b
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.linux%s
   --  system.multiprocessors%s
   --  system.multiprocessors%b
   --  system.os_constants%s
   --  system.os_interface%s
   --  system.os_interface%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.finalization_masters%b
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.storage_pools.subpools%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.task_primitives%s
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking.debug%s
   --  system.tasking%b
   --  system.task_primitives.operations%b
   --  system.tasking.debug%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.delays%s
   --  ada.calendar.delays%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.tasking.initialization%s
   --  system.tasking.task_attributes%s
   --  system.tasking.initialization%b
   --  system.tasking.task_attributes%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.protected_objects.entries%s
   --  system.tasking.protected_objects.entries%b
   --  system.tasking.queuing%s
   --  system.tasking.queuing%b
   --  system.tasking.utilities%s
   --  system.tasking.utilities%b
   --  system.tasking.entry_calls%s
   --  system.tasking.rendezvous%s
   --  system.tasking.protected_objects.operations%s
   --  system.tasking.protected_objects.operations%b
   --  system.tasking.entry_calls%b
   --  system.tasking.rendezvous%b
   --  system.tasking.stages%s
   --  system.tasking.stages%b
   --  rb%s
   --  rb%b
   --  main%b
   --  END ELABORATION ORDER

end ada_main;
