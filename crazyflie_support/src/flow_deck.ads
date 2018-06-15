--------------------------------------------
--    F L O W - D E C K    D R I V E R    --
--------------------------------------------

--  Derived from flow_deck.c of the Crazyflie frimware

with Types;     use Types;

package Flow_Deck is

   --  Global variables and constants
   --  For flow measurement
   AVERAGE_HISTORY_LENGTH : constant := 4;
   OUTLIER_LIMIT          : constant := 100;
   LP_CONSTANT            : constant Float := 0.8;
   USE_LP_FILTER          : constant Boolean := False;
   USE_MA_SMOOTHING       : constant Boolean := False;
   --  Disables pushing the flow measurement in the EKF
   Use_Flow_Disabled      : Boolean := False;
   
   --  For the Z_Ranger
   RANGE_OUTLIER_LIMIT    : constant := 3000; --  in [mm]

   subtype Pixel_Average is Float_Array (1 .. AVERAGE_HISTORY_LENGTH);

   type T_Pixel_Averages is record
      Average_X : Pixel_Average := (others => 0.0);
      Average_Y : Pixel_Average := (others => 0.0);
      ptr       : Natural := 1;
   end record;

   type MB_FF0 is new T_Uint8 range 0 .. 1;
   for MB_FF0'Size use 1;

   type MB_Run_Mode is new T_Uint8 range 0 .. 3;
   for MB_Run_Mode'Size use 2;

   type MB_Reserved_1 is new T_Uint8 range 0 .. 1;
   for MB_Reserved_1'Size use 1;

   type MB_RF0 is new T_Uint8 range 0 .. 1;
   for MB_RF0'Size use 1;

   type MB_Reserved_2 is new T_Uint8 range 0 .. 3;
   for MB_Reserved_2'Size use 2;

   type MB_Motion_Occured is new T_Uint8 range 0 .. 1;
   for MB_Motion_Occured'Size use 1;

   RAW_SIZE : constant := 12;

   type Motion_Burst_Raw is array (1 .. RAW_SIZE) of T_Uint8;

   type Motion_Burst_Representation is (DETAILED, MOTION_BYTE, RAW);

   type Motion_Burst (Repr : Motion_Burst_Representation := DETAILED) is record
      case Repr is
         when RAW =>
            Raw            : Motion_Burst_Raw;

         when DETAILED =>
            Frame_From_0   : MB_FF0;
            Run_Mode       : MB_Run_Mode;
            Reserved_1     : MB_Reserved_1;
            Raw_From_0     : MB_RF0;
            Reserver_2     : MB_Reserved_2;
            Motion_Occured : MB_Motion_Occured;

            Observation_1  : T_Uint8;
            Delta_X_1      : T_Int16;
            Delta_Y_1      : T_Int16;
            Squal_1        : T_Uint8;
            Raw_Data_Sum_1 : T_Uint8;
            Max_Raw_Data_1 : T_Uint8;
            Min_Raw_Data_1 : T_Uint8;
            Shutter_1      : T_Uint16;

         when MOTION_BYTE =>
            Motion         : T_Uint8;

            Observation_2  : T_Uint8;
            Delta_X_2      : T_Int16;
            Delta_Y_2      : T_Int16;
            Squal_2        : T_Uint8;
            Raw_Data_Sum_2 : T_Uint8;
            Max_Raw_Data_2 : T_Uint8;
            Min_Raw_Data_2 : T_Uint8;
            Shutter_2      : T_Uint16;
      end case;

   end record;

   pragma Unchecked_Union (Motion_Burst);
   for Motion_Burst'Size use 96;
   pragma Pack (Motion_Burst);

   --  Flow Measurement
   subtype dpixel_Data is Float_Array (1 .. 2);

   type Flow_Measurement_Representation is (DETAILED, LIST);

   type Flow_Measurement (Repr : Flow_Measurement_Representation := DETAILED) is record
       --  Timestamp : Time;
       --  Measurement standard deviation
      std_Dev_X : Float;
      std_Dev_Y : Float;
      --  Time during which pixels were accumulated
      dt        : Float;
      
      --  Accumulated pixel count
      case Repr is
         when DETAILED =>
            dpixel_X : Float;
            dpixel_Y : Float;
         when LIST =>
            dpixel : dpixel_Data;
      end case;
   end record;

   pragma Unchecked_Union (Flow_Measurement);
   --  for Flow_Measurement'Size use 32+5*Float'Size;
   
   --  TOF Measurement
   type TOF_Measurement is record
      --  Timestamp : Time;
      Distance  : Float;
      std_Dev   : Float;
   end record;  
   
   --  Procedures and functions
   procedure Z_Ranger_Init;
   procedure PaMotion_Init;
   procedure Flow_Deck_Init;

   function Z_Ranger_Test return Boolean;
   function PaMotion_Test return Boolean;
   function Flow_Deck_Test return Boolean;  

   procedure Z_Ranger_Task (TOF_Data : access TOF_Measurement);
   procedure PaMotion_Task (Flow_Data : access Flow_Measurement);   

   function Add_Motion_To_LOG return Boolean;

   function Add_Motion_To_Parameter return Boolean;
   

private

   --  Global variables
   Z_Ranger_Is_Init : Boolean := False;
   PaMotion_Is_Init : Boolean := False;
   Is_Init          : Boolean := False;
   Outlier_Count    : T_Uint8 := 0;
   --  Global variable used to read motion
   Current_Motion   : access Motion_Burst := new Motion_Burst;


   --  procedures and functions
   procedure Register_Write (Reg : T_Uint8; Value : T_Uint8);
   function Register_Read (Reg : T_Uint8) return T_Uint8;
   procedure Read_Motion (Motion : access Motion_Burst);
   

end Flow_Deck;
