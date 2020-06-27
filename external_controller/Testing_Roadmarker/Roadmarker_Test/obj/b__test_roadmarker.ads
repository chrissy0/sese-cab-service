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
                    "GNAT Version: Community 2019 (20190517-83)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_test_roadmarker" & ASCII.NUL;
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
   u00001 : constant Version_32 := 16#1e5bcf78#;
   pragma Export (C, u00001, "test_roadmarkerB");
   u00002 : constant Version_32 := 16#050ff2f0#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#35869f17#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#83bb8e3b#;
   pragma Export (C, u00004, "aunitB");
   u00005 : constant Version_32 := 16#76cdf7c6#;
   pragma Export (C, u00005, "aunitS");
   u00006 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00006, "adaS");
   u00007 : constant Version_32 := 16#b6c145a2#;
   pragma Export (C, u00007, "aunit__memoryB");
   u00008 : constant Version_32 := 16#e45c27b8#;
   pragma Export (C, u00008, "aunit__memoryS");
   u00009 : constant Version_32 := 16#32a08138#;
   pragma Export (C, u00009, "systemS");
   u00010 : constant Version_32 := 16#ced09590#;
   pragma Export (C, u00010, "system__storage_elementsB");
   u00011 : constant Version_32 := 16#1f63cb3c#;
   pragma Export (C, u00011, "system__storage_elementsS");
   u00012 : constant Version_32 := 16#17d955ab#;
   pragma Export (C, u00012, "aunit__reporterS");
   u00013 : constant Version_32 := 16#90189159#;
   pragma Export (C, u00013, "ada__exceptionsB");
   u00014 : constant Version_32 := 16#2ccb9557#;
   pragma Export (C, u00014, "ada__exceptionsS");
   u00015 : constant Version_32 := 16#5726abed#;
   pragma Export (C, u00015, "ada__exceptions__last_chance_handlerB");
   u00016 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00016, "ada__exceptions__last_chance_handlerS");
   u00017 : constant Version_32 := 16#ae860117#;
   pragma Export (C, u00017, "system__soft_linksB");
   u00018 : constant Version_32 := 16#77a38a8e#;
   pragma Export (C, u00018, "system__soft_linksS");
   u00019 : constant Version_32 := 16#87be2c0f#;
   pragma Export (C, u00019, "system__secondary_stackB");
   u00020 : constant Version_32 := 16#77347921#;
   pragma Export (C, u00020, "system__secondary_stackS");
   u00021 : constant Version_32 := 16#86dbf443#;
   pragma Export (C, u00021, "system__parametersB");
   u00022 : constant Version_32 := 16#7a4cd513#;
   pragma Export (C, u00022, "system__parametersS");
   u00023 : constant Version_32 := 16#75bf515c#;
   pragma Export (C, u00023, "system__soft_links__initializeB");
   u00024 : constant Version_32 := 16#5697fc2b#;
   pragma Export (C, u00024, "system__soft_links__initializeS");
   u00025 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00025, "system__stack_checkingB");
   u00026 : constant Version_32 := 16#bc1fead0#;
   pragma Export (C, u00026, "system__stack_checkingS");
   u00027 : constant Version_32 := 16#34742901#;
   pragma Export (C, u00027, "system__exception_tableB");
   u00028 : constant Version_32 := 16#6f0ee87a#;
   pragma Export (C, u00028, "system__exception_tableS");
   u00029 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00029, "system__exceptionsB");
   u00030 : constant Version_32 := 16#5ac3ecce#;
   pragma Export (C, u00030, "system__exceptionsS");
   u00031 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00031, "system__exceptions__machineB");
   u00032 : constant Version_32 := 16#d27d9682#;
   pragma Export (C, u00032, "system__exceptions__machineS");
   u00033 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00033, "system__exceptions_debugB");
   u00034 : constant Version_32 := 16#4c2a78fc#;
   pragma Export (C, u00034, "system__exceptions_debugS");
   u00035 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00035, "system__img_intB");
   u00036 : constant Version_32 := 16#307b61fa#;
   pragma Export (C, u00036, "system__img_intS");
   u00037 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00037, "system__tracebackB");
   u00038 : constant Version_32 := 16#6c825ffc#;
   pragma Export (C, u00038, "system__tracebackS");
   u00039 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00039, "system__traceback_entriesB");
   u00040 : constant Version_32 := 16#32fb7748#;
   pragma Export (C, u00040, "system__traceback_entriesS");
   u00041 : constant Version_32 := 16#9ad5ad12#;
   pragma Export (C, u00041, "system__traceback__symbolicB");
   u00042 : constant Version_32 := 16#c84061d1#;
   pragma Export (C, u00042, "system__traceback__symbolicS");
   u00043 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00043, "ada__containersS");
   u00044 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00044, "ada__exceptions__tracebackB");
   u00045 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00045, "ada__exceptions__tracebackS");
   u00046 : constant Version_32 := 16#e865e681#;
   pragma Export (C, u00046, "system__bounded_stringsB");
   u00047 : constant Version_32 := 16#455da021#;
   pragma Export (C, u00047, "system__bounded_stringsS");
   u00048 : constant Version_32 := 16#74f70e62#;
   pragma Export (C, u00048, "system__crtlS");
   u00049 : constant Version_32 := 16#cf32f6f7#;
   pragma Export (C, u00049, "system__dwarf_linesB");
   u00050 : constant Version_32 := 16#eeedbcbd#;
   pragma Export (C, u00050, "system__dwarf_linesS");
   u00051 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00051, "ada__charactersS");
   u00052 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00052, "ada__characters__handlingB");
   u00053 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00053, "ada__characters__handlingS");
   u00054 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00054, "ada__characters__latin_1S");
   u00055 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00055, "ada__stringsS");
   u00056 : constant Version_32 := 16#96df1a3f#;
   pragma Export (C, u00056, "ada__strings__mapsB");
   u00057 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00057, "ada__strings__mapsS");
   u00058 : constant Version_32 := 16#a21ad5cd#;
   pragma Export (C, u00058, "system__bit_opsB");
   u00059 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00059, "system__bit_opsS");
   u00060 : constant Version_32 := 16#0626fdbb#;
   pragma Export (C, u00060, "system__unsigned_typesS");
   u00061 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00061, "ada__strings__maps__constantsS");
   u00062 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00062, "interfacesS");
   u00063 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00063, "system__address_imageB");
   u00064 : constant Version_32 := 16#934c1c02#;
   pragma Export (C, u00064, "system__address_imageS");
   u00065 : constant Version_32 := 16#ec78c2bf#;
   pragma Export (C, u00065, "system__img_unsB");
   u00066 : constant Version_32 := 16#99d2c14c#;
   pragma Export (C, u00066, "system__img_unsS");
   u00067 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00067, "system__ioB");
   u00068 : constant Version_32 := 16#ace27677#;
   pragma Export (C, u00068, "system__ioS");
   u00069 : constant Version_32 := 16#3080f2ca#;
   pragma Export (C, u00069, "system__mmapB");
   u00070 : constant Version_32 := 16#08d13e5f#;
   pragma Export (C, u00070, "system__mmapS");
   u00071 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00071, "ada__io_exceptionsS");
   u00072 : constant Version_32 := 16#a82e20f9#;
   pragma Export (C, u00072, "system__mmap__os_interfaceB");
   u00073 : constant Version_32 := 16#8f4541b8#;
   pragma Export (C, u00073, "system__mmap__os_interfaceS");
   u00074 : constant Version_32 := 16#de8ca4eb#;
   pragma Export (C, u00074, "system__os_libB");
   u00075 : constant Version_32 := 16#d8e681fb#;
   pragma Export (C, u00075, "system__os_libS");
   u00076 : constant Version_32 := 16#ec4d5631#;
   pragma Export (C, u00076, "system__case_utilB");
   u00077 : constant Version_32 := 16#0d75376c#;
   pragma Export (C, u00077, "system__case_utilS");
   u00078 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00078, "system__stringsB");
   u00079 : constant Version_32 := 16#52b6adad#;
   pragma Export (C, u00079, "system__stringsS");
   u00080 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00080, "interfaces__cB");
   u00081 : constant Version_32 := 16#467817d8#;
   pragma Export (C, u00081, "interfaces__cS");
   u00082 : constant Version_32 := 16#5a3f5337#;
   pragma Export (C, u00082, "system__object_readerB");
   u00083 : constant Version_32 := 16#f6d45c39#;
   pragma Export (C, u00083, "system__object_readerS");
   u00084 : constant Version_32 := 16#fb020d94#;
   pragma Export (C, u00084, "system__val_lliB");
   u00085 : constant Version_32 := 16#5ece13c8#;
   pragma Export (C, u00085, "system__val_lliS");
   u00086 : constant Version_32 := 16#b8e72903#;
   pragma Export (C, u00086, "system__val_lluB");
   u00087 : constant Version_32 := 16#6be87059#;
   pragma Export (C, u00087, "system__val_lluS");
   u00088 : constant Version_32 := 16#269742a9#;
   pragma Export (C, u00088, "system__val_utilB");
   u00089 : constant Version_32 := 16#9e0037c6#;
   pragma Export (C, u00089, "system__val_utilS");
   u00090 : constant Version_32 := 16#d7bf3f29#;
   pragma Export (C, u00090, "system__exception_tracesB");
   u00091 : constant Version_32 := 16#167fa1a2#;
   pragma Export (C, u00091, "system__exception_tracesS");
   u00092 : constant Version_32 := 16#f66d6445#;
   pragma Export (C, u00092, "system__win32S");
   u00093 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00093, "system__wch_conB");
   u00094 : constant Version_32 := 16#29dda3ea#;
   pragma Export (C, u00094, "system__wch_conS");
   u00095 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00095, "system__wch_stwB");
   u00096 : constant Version_32 := 16#04cc8feb#;
   pragma Export (C, u00096, "system__wch_stwS");
   u00097 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00097, "system__wch_cnvB");
   u00098 : constant Version_32 := 16#266a1919#;
   pragma Export (C, u00098, "system__wch_cnvS");
   u00099 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00099, "system__wch_jisB");
   u00100 : constant Version_32 := 16#a61a0038#;
   pragma Export (C, u00100, "system__wch_jisS");
   u00101 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00101, "ada__streamsB");
   u00102 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00102, "ada__streamsS");
   u00103 : constant Version_32 := 16#d398a95f#;
   pragma Export (C, u00103, "ada__tagsB");
   u00104 : constant Version_32 := 16#12a0afb8#;
   pragma Export (C, u00104, "ada__tagsS");
   u00105 : constant Version_32 := 16#796f31f1#;
   pragma Export (C, u00105, "system__htableB");
   u00106 : constant Version_32 := 16#b66232d2#;
   pragma Export (C, u00106, "system__htableS");
   u00107 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00107, "system__string_hashB");
   u00108 : constant Version_32 := 16#143c59ac#;
   pragma Export (C, u00108, "system__string_hashS");
   u00109 : constant Version_32 := 16#e99cd447#;
   pragma Export (C, u00109, "aunit__optionsS");
   u00110 : constant Version_32 := 16#e9d6512d#;
   pragma Export (C, u00110, "aunit__test_filtersB");
   u00111 : constant Version_32 := 16#9a67cba8#;
   pragma Export (C, u00111, "aunit__test_filtersS");
   u00112 : constant Version_32 := 16#6e9501f4#;
   pragma Export (C, u00112, "aunit__simple_test_casesB");
   u00113 : constant Version_32 := 16#f9679d50#;
   pragma Export (C, u00113, "aunit__simple_test_casesS");
   u00114 : constant Version_32 := 16#8872fb1a#;
   pragma Export (C, u00114, "aunit__assertionsB");
   u00115 : constant Version_32 := 16#3b7b7e5b#;
   pragma Export (C, u00115, "aunit__assertionsS");
   u00116 : constant Version_32 := 16#2731b3e7#;
   pragma Export (C, u00116, "ada_containers__aunit_listsB");
   u00117 : constant Version_32 := 16#c8d9569a#;
   pragma Export (C, u00117, "ada_containers__aunit_listsS");
   u00118 : constant Version_32 := 16#11329e00#;
   pragma Export (C, u00118, "ada_containersS");
   u00119 : constant Version_32 := 16#9b1c7ff2#;
   pragma Export (C, u00119, "aunit__memory__utilsB");
   u00120 : constant Version_32 := 16#fb2f6c57#;
   pragma Export (C, u00120, "aunit__memory__utilsS");
   u00121 : constant Version_32 := 16#01adf261#;
   pragma Export (C, u00121, "aunit__test_resultsB");
   u00122 : constant Version_32 := 16#1087836e#;
   pragma Export (C, u00122, "aunit__test_resultsS");
   u00123 : constant Version_32 := 16#9df5edcf#;
   pragma Export (C, u00123, "aunit__time_measureB");
   u00124 : constant Version_32 := 16#99399b1d#;
   pragma Export (C, u00124, "aunit__time_measureS");
   u00125 : constant Version_32 := 16#fc54e290#;
   pragma Export (C, u00125, "ada__calendarB");
   u00126 : constant Version_32 := 16#31350a81#;
   pragma Export (C, u00126, "ada__calendarS");
   u00127 : constant Version_32 := 16#24ec69e6#;
   pragma Export (C, u00127, "system__os_primitivesB");
   u00128 : constant Version_32 := 16#355de4ce#;
   pragma Export (C, u00128, "system__os_primitivesS");
   u00129 : constant Version_32 := 16#05c60a38#;
   pragma Export (C, u00129, "system__task_lockB");
   u00130 : constant Version_32 := 16#532ab656#;
   pragma Export (C, u00130, "system__task_lockS");
   u00131 : constant Version_32 := 16#3d84d1b9#;
   pragma Export (C, u00131, "system__win32__extS");
   u00132 : constant Version_32 := 16#6b6cea8f#;
   pragma Export (C, u00132, "aunit__testsS");
   u00133 : constant Version_32 := 16#d96e3c40#;
   pragma Export (C, u00133, "system__finalization_mastersB");
   u00134 : constant Version_32 := 16#695cb8f2#;
   pragma Export (C, u00134, "system__finalization_mastersS");
   u00135 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00135, "system__img_boolB");
   u00136 : constant Version_32 := 16#c779f0d3#;
   pragma Export (C, u00136, "system__img_boolS");
   u00137 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00137, "ada__finalizationS");
   u00138 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00138, "system__finalization_rootB");
   u00139 : constant Version_32 := 16#7d52f2a8#;
   pragma Export (C, u00139, "system__finalization_rootS");
   u00140 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00140, "system__storage_poolsB");
   u00141 : constant Version_32 := 16#114d1f95#;
   pragma Export (C, u00141, "system__storage_poolsS");
   u00142 : constant Version_32 := 16#5a895de2#;
   pragma Export (C, u00142, "system__pool_globalB");
   u00143 : constant Version_32 := 16#7141203e#;
   pragma Export (C, u00143, "system__pool_globalS");
   u00144 : constant Version_32 := 16#e31b7c4e#;
   pragma Export (C, u00144, "system__memoryB");
   u00145 : constant Version_32 := 16#6bdde70c#;
   pragma Export (C, u00145, "system__memoryS");
   u00146 : constant Version_32 := 16#b5988c27#;
   pragma Export (C, u00146, "gnatS");
   u00147 : constant Version_32 := 16#9c857b76#;
   pragma Export (C, u00147, "gnat__source_infoS");
   u00148 : constant Version_32 := 16#ea75efa1#;
   pragma Export (C, u00148, "gnat__tracebackB");
   u00149 : constant Version_32 := 16#8b785f28#;
   pragma Export (C, u00149, "gnat__tracebackS");
   u00150 : constant Version_32 := 16#4b271bfa#;
   pragma Export (C, u00150, "gnat__traceback__symbolicS");
   u00151 : constant Version_32 := 16#fbb5441f#;
   pragma Export (C, u00151, "aunit__reporter__xmlB");
   u00152 : constant Version_32 := 16#1638e492#;
   pragma Export (C, u00152, "aunit__reporter__xmlS");
   u00153 : constant Version_32 := 16#b48102f5#;
   pragma Export (C, u00153, "gnat__ioB");
   u00154 : constant Version_32 := 16#2a95b695#;
   pragma Export (C, u00154, "gnat__ioS");
   u00155 : constant Version_32 := 16#b602a99c#;
   pragma Export (C, u00155, "system__exn_intB");
   u00156 : constant Version_32 := 16#702c9d64#;
   pragma Export (C, u00156, "system__exn_intS");
   u00157 : constant Version_32 := 16#e11af2d7#;
   pragma Export (C, u00157, "aunit__runB");
   u00158 : constant Version_32 := 16#4b2a8016#;
   pragma Export (C, u00158, "aunit__runS");
   u00159 : constant Version_32 := 16#276e73f2#;
   pragma Export (C, u00159, "aunit__test_suitesB");
   u00160 : constant Version_32 := 16#f3c7e671#;
   pragma Export (C, u00160, "aunit__test_suitesS");
   u00161 : constant Version_32 := 16#764e9d9b#;
   pragma Export (C, u00161, "roadmarker_suiteB");
   u00162 : constant Version_32 := 16#0e8234c6#;
   pragma Export (C, u00162, "roadmarker_suiteS");
   u00163 : constant Version_32 := 16#78683681#;
   pragma Export (C, u00163, "aunit__test_callerB");
   u00164 : constant Version_32 := 16#581d22b8#;
   pragma Export (C, u00164, "aunit__test_callerS");
   u00165 : constant Version_32 := 16#269b1972#;
   pragma Export (C, u00165, "aunit__test_fixturesB");
   u00166 : constant Version_32 := 16#3b99f1a5#;
   pragma Export (C, u00166, "aunit__test_fixturesS");
   u00167 : constant Version_32 := 16#43d88962#;
   pragma Export (C, u00167, "roadmarker_testB");
   u00168 : constant Version_32 := 16#aa4d140f#;
   pragma Export (C, u00168, "roadmarker_testS");
   u00169 : constant Version_32 := 16#357666d8#;
   pragma Export (C, u00169, "ada__calendar__delaysB");
   u00170 : constant Version_32 := 16#d86d2f1d#;
   pragma Export (C, u00170, "ada__calendar__delaysS");
   u00171 : constant Version_32 := 16#a49e210b#;
   pragma Export (C, u00171, "ada__task_identificationB");
   u00172 : constant Version_32 := 16#fdedad8d#;
   pragma Export (C, u00172, "ada__task_identificationS");
   u00173 : constant Version_32 := 16#5901abf2#;
   pragma Export (C, u00173, "system__task_primitivesS");
   u00174 : constant Version_32 := 16#5bbb030d#;
   pragma Export (C, u00174, "system__os_interfaceS");
   u00175 : constant Version_32 := 16#1d638357#;
   pragma Export (C, u00175, "interfaces__c__stringsB");
   u00176 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00176, "interfaces__c__stringsS");
   u00177 : constant Version_32 := 16#c1984f17#;
   pragma Export (C, u00177, "system__task_primitives__operationsB");
   u00178 : constant Version_32 := 16#17a57953#;
   pragma Export (C, u00178, "system__task_primitives__operationsS");
   u00179 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00179, "system__float_controlB");
   u00180 : constant Version_32 := 16#d25cc204#;
   pragma Export (C, u00180, "system__float_controlS");
   u00181 : constant Version_32 := 16#6387a759#;
   pragma Export (C, u00181, "system__interrupt_managementB");
   u00182 : constant Version_32 := 16#b66bc35d#;
   pragma Export (C, u00182, "system__interrupt_managementS");
   u00183 : constant Version_32 := 16#f65595cf#;
   pragma Export (C, u00183, "system__multiprocessorsB");
   u00184 : constant Version_32 := 16#0a0c1e4b#;
   pragma Export (C, u00184, "system__multiprocessorsS");
   u00185 : constant Version_32 := 16#ce7dfb56#;
   pragma Export (C, u00185, "system__task_infoB");
   u00186 : constant Version_32 := 16#c2531eac#;
   pragma Export (C, u00186, "system__task_infoS");
   u00187 : constant Version_32 := 16#4f04e3f2#;
   pragma Export (C, u00187, "system__taskingB");
   u00188 : constant Version_32 := 16#76fd1509#;
   pragma Export (C, u00188, "system__taskingS");
   u00189 : constant Version_32 := 16#6ec3c867#;
   pragma Export (C, u00189, "system__stack_usageB");
   u00190 : constant Version_32 := 16#3a3ac346#;
   pragma Export (C, u00190, "system__stack_usageS");
   u00191 : constant Version_32 := 16#e5a48551#;
   pragma Export (C, u00191, "system__tasking__debugB");
   u00192 : constant Version_32 := 16#48f9280e#;
   pragma Export (C, u00192, "system__tasking__debugS");
   u00193 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00193, "system__concat_2B");
   u00194 : constant Version_32 := 16#300056e8#;
   pragma Export (C, u00194, "system__concat_2S");
   u00195 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00195, "system__concat_3B");
   u00196 : constant Version_32 := 16#39d0dd9d#;
   pragma Export (C, u00196, "system__concat_3S");
   u00197 : constant Version_32 := 16#273384e4#;
   pragma Export (C, u00197, "system__img_enum_newB");
   u00198 : constant Version_32 := 16#53ec87f8#;
   pragma Export (C, u00198, "system__img_enum_newS");
   u00199 : constant Version_32 := 16#66acc56c#;
   pragma Export (C, u00199, "system__tasking__utilitiesB");
   u00200 : constant Version_32 := 16#97705b7e#;
   pragma Export (C, u00200, "system__tasking__utilitiesS");
   u00201 : constant Version_32 := 16#ff8eddb4#;
   pragma Export (C, u00201, "system__tasking__initializationB");
   u00202 : constant Version_32 := 16#fc2303e6#;
   pragma Export (C, u00202, "system__tasking__initializationS");
   u00203 : constant Version_32 := 16#56c470a2#;
   pragma Export (C, u00203, "system__soft_links__taskingB");
   u00204 : constant Version_32 := 16#e939497e#;
   pragma Export (C, u00204, "system__soft_links__taskingS");
   u00205 : constant Version_32 := 16#17d21067#;
   pragma Export (C, u00205, "ada__exceptions__is_null_occurrenceB");
   u00206 : constant Version_32 := 16#e1d7566f#;
   pragma Export (C, u00206, "ada__exceptions__is_null_occurrenceS");
   u00207 : constant Version_32 := 16#0f96c370#;
   pragma Export (C, u00207, "system__tasking__task_attributesB");
   u00208 : constant Version_32 := 16#e81a3c25#;
   pragma Export (C, u00208, "system__tasking__task_attributesS");
   u00209 : constant Version_32 := 16#43cda1ce#;
   pragma Export (C, u00209, "system__tasking__queuingB");
   u00210 : constant Version_32 := 16#6dba2805#;
   pragma Export (C, u00210, "system__tasking__queuingS");
   u00211 : constant Version_32 := 16#f24a7f45#;
   pragma Export (C, u00211, "system__tasking__protected_objectsB");
   u00212 : constant Version_32 := 16#15001baf#;
   pragma Export (C, u00212, "system__tasking__protected_objectsS");
   u00213 : constant Version_32 := 16#ff485338#;
   pragma Export (C, u00213, "system__tasking__protected_objects__entriesB");
   u00214 : constant Version_32 := 16#7daf93e7#;
   pragma Export (C, u00214, "system__tasking__protected_objects__entriesS");
   u00215 : constant Version_32 := 16#100eaf58#;
   pragma Export (C, u00215, "system__restrictionsB");
   u00216 : constant Version_32 := 16#79d25869#;
   pragma Export (C, u00216, "system__restrictionsS");
   u00217 : constant Version_32 := 16#f4e097a7#;
   pragma Export (C, u00217, "ada__text_ioB");
   u00218 : constant Version_32 := 16#03e83e15#;
   pragma Export (C, u00218, "ada__text_ioS");
   u00219 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00219, "interfaces__c_streamsB");
   u00220 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00220, "interfaces__c_streamsS");
   u00221 : constant Version_32 := 16#ec083f01#;
   pragma Export (C, u00221, "system__file_ioB");
   u00222 : constant Version_32 := 16#95d1605d#;
   pragma Export (C, u00222, "system__file_ioS");
   u00223 : constant Version_32 := 16#cf3f1b90#;
   pragma Export (C, u00223, "system__file_control_blockS");
   u00224 : constant Version_32 := 16#7532474a#;
   pragma Export (C, u00224, "roadmarkerB");
   u00225 : constant Version_32 := 16#2dc8e689#;
   pragma Export (C, u00225, "roadmarkerS");
   u00226 : constant Version_32 := 16#38c7c529#;
   pragma Export (C, u00226, "roadmarker_functionsB");
   u00227 : constant Version_32 := 16#0febe8a2#;
   pragma Export (C, u00227, "roadmarker_functionsS");
   u00228 : constant Version_32 := 16#9cbb0d81#;
   pragma Export (C, u00228, "system__tasking__stagesB");
   u00229 : constant Version_32 := 16#5fc08d5c#;
   pragma Export (C, u00229, "system__tasking__stagesS");
   u00230 : constant Version_32 := 16#1e07ca19#;
   pragma Export (C, u00230, "system__tasking__rendezvousB");
   u00231 : constant Version_32 := 16#5618a4d0#;
   pragma Export (C, u00231, "system__tasking__rendezvousS");
   u00232 : constant Version_32 := 16#e49898fa#;
   pragma Export (C, u00232, "system__tasking__entry_callsB");
   u00233 : constant Version_32 := 16#6342024e#;
   pragma Export (C, u00233, "system__tasking__entry_callsS");
   u00234 : constant Version_32 := 16#0eed7110#;
   pragma Export (C, u00234, "system__tasking__protected_objects__operationsB");
   u00235 : constant Version_32 := 16#ba36ad85#;
   pragma Export (C, u00235, "system__tasking__protected_objects__operationsS");
   u00236 : constant Version_32 := 16#64176fe3#;
   pragma Export (C, u00236, "ada__real_timeB");
   u00237 : constant Version_32 := 16#69ea8064#;
   pragma Export (C, u00237, "ada__real_timeS");
   u00238 : constant Version_32 := 16#b39adfed#;
   pragma Export (C, u00238, "system__tasking__async_delaysB");
   u00239 : constant Version_32 := 16#fbf0123d#;
   pragma Export (C, u00239, "system__tasking__async_delaysS");
   u00240 : constant Version_32 := 16#0110e3c6#;
   pragma Export (C, u00240, "system__interrupt_management__operationsB");
   u00241 : constant Version_32 := 16#19b909c9#;
   pragma Export (C, u00241, "system__interrupt_management__operationsS");
   u00242 : constant Version_32 := 16#2e260032#;
   pragma Export (C, u00242, "system__storage_pools__subpoolsB");
   u00243 : constant Version_32 := 16#cc5a1856#;
   pragma Export (C, u00243, "system__storage_pools__subpoolsS");
   u00244 : constant Version_32 := 16#84042202#;
   pragma Export (C, u00244, "system__storage_pools__subpools__finalizationB");
   u00245 : constant Version_32 := 16#fe2f4b3a#;
   pragma Export (C, u00245, "system__storage_pools__subpools__finalizationS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.exn_int%s
   --  system.exn_int%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.io%s
   --  system.io%b
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
   --  ada.characters.handling%s
   --  system.case_util%s
   --  system.os_lib%s
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.val_lli%s
   --  system.val_llu%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.bounded_strings%s
   --  system.bounded_strings%b
   --  system.case_util%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.win32%s
   --  ada.characters.handling%b
   --  system.exception_traces%s
   --  system.exception_traces%b
   --  system.memory%s
   --  system.memory%b
   --  system.mmap%s
   --  system.mmap.os_interface%s
   --  system.mmap.os_interface%b
   --  system.mmap%b
   --  system.object_reader%s
   --  system.object_reader%b
   --  system.dwarf_lines%s
   --  system.dwarf_lines%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  system.val_lli%b
   --  system.val_llu%b
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  gnat%s
   --  gnat.io%s
   --  gnat.io%b
   --  gnat.source_info%s
   --  interfaces.c.strings%s
   --  interfaces.c.strings%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.multiprocessors%s
   --  system.multiprocessors%b
   --  system.os_interface%s
   --  system.interrupt_management%s
   --  system.interrupt_management%b
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
   --  system.task_lock%s
   --  system.task_lock%b
   --  system.task_primitives%s
   --  system.win32.ext%s
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%b
   --  system.tasking%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.delays%s
   --  ada.calendar.delays%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  gnat.traceback%s
   --  gnat.traceback%b
   --  gnat.traceback.symbolic%s
   --  system.interrupt_management.operations%s
   --  system.interrupt_management.operations%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.tasking.initialization%s
   --  system.tasking.task_attributes%s
   --  system.tasking.task_attributes%b
   --  system.tasking.initialization%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.protected_objects.entries%s
   --  system.tasking.protected_objects.entries%b
   --  system.tasking.queuing%s
   --  system.tasking.queuing%b
   --  system.tasking.utilities%s
   --  system.tasking.utilities%b
   --  ada.task_identification%s
   --  ada.task_identification%b
   --  system.tasking.entry_calls%s
   --  system.tasking.rendezvous%s
   --  system.tasking.protected_objects.operations%s
   --  system.tasking.protected_objects.operations%b
   --  system.tasking.entry_calls%b
   --  system.tasking.rendezvous%b
   --  system.tasking.stages%s
   --  system.tasking.stages%b
   --  system.tasking.async_delays%s
   --  system.tasking.async_delays%b
   --  ada_containers%s
   --  aunit%s
   --  aunit.memory%s
   --  aunit.memory%b
   --  aunit%b
   --  aunit.memory.utils%s
   --  aunit.memory.utils%b
   --  ada_containers.aunit_lists%s
   --  ada_containers.aunit_lists%b
   --  aunit.tests%s
   --  aunit.time_measure%s
   --  aunit.time_measure%b
   --  aunit.test_results%s
   --  aunit.test_results%b
   --  aunit.assertions%s
   --  aunit.assertions%b
   --  aunit.test_filters%s
   --  aunit.options%s
   --  aunit.simple_test_cases%s
   --  aunit.simple_test_cases%b
   --  aunit.test_filters%b
   --  aunit.reporter%s
   --  aunit.reporter.xml%s
   --  aunit.reporter.xml%b
   --  aunit.test_fixtures%s
   --  aunit.test_fixtures%b
   --  aunit.test_caller%s
   --  aunit.test_caller%b
   --  aunit.test_suites%s
   --  aunit.test_suites%b
   --  aunit.run%s
   --  aunit.run%b
   --  roadmarker_functions%s
   --  roadmarker_functions%b
   --  roadmarker%s
   --  roadmarker%b
   --  roadmarker_test%s
   --  roadmarker_test%b
   --  roadmarker_suite%s
   --  roadmarker_suite%b
   --  test_roadmarker%b
   --  END ELABORATION ORDER

end ada_main;
