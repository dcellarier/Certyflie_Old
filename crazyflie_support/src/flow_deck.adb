--------------------------------------------
--    F L O W - D E C K    D R I V E R    --
--------------------------------------------

--  Derived from flow_deck.c and zranger.c
--  of the Crazyflie frimware

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Real_Time;                     use Ada.Real_Time;
with Ada.Unchecked_Conversion;

with Log;               use Log;
with Parameter;         use Parameter;
with STM32.SPI.DECK;    use STM32.SPI.DECK;
with STM32.I2C;         use STM32.I2C;
with STM32.Board;       use STM32.Board;
with STM32.GPIO;        use STM32.GPIO;
with HAL.GPIO;
with HAL;
with Maths;             use Maths;

with Interfaces;        use Interfaces;
--  with EXT_UART;          use EXT_UART;

-- Z_Ranger driver
with VL53L0X;           use VL53L0X;

package body Flow_Deck is

   NCS_PIN : GPIO_Point renames EXT_CS1;
   -- Flow measurement variables
   dpixel_X_Previous : Float := 0.0;
   dpixel_Y_Previous : Float := 0.0;
   Pixel_Averages    : T_Pixel_Averages;
   --  Flow_Data : access Flow_Measurement
   --    := new Flow_Measurement'(Repr => DETAILED,
   --                             std_Dev_X => 0.25,
   --                             std_Dev_Y => 0.25,
   --                             dt => 0.1,
   --                             dpixel_X => 0.0,
   --                             dpixel_Y => 0.0);

   --  Define Z_Ranger device
   Z_Ranger_Device : VL53L0X_Ranging_Sensor (I2C_EXT_Port'Access);
   Last_Range   : T_Uint16 := 0;
   --  TOF_Data        : access TOF_Measurement := new TOF_Measurement;
   --  Measurement noise model for the Z_Ranger
   exp_Point_A     : constant Float := 1.0;
   exp_Std_A       : constant Float := 0.0025; -- STD at elevation expPointA [m]
   exp_Point_B     : constant Float := 1.3;
   exp_Std_B       : constant Float := 0.2;    -- STD at elevation expPointB [m]
   exp_Coeff       : Float;


   ----------------------
   --  Register_Write  --
   ----------------------
   procedure Register_Write (Reg : T_Uint8; Value : T_Uint8) is
      Data_Tx : SPI_DMA_Data;
      Data_Rx : SPI_DMA_Data;
      Reg_Write : T_Uint8;
   begin
      --  Set MSB to 1 for write
      Reg_Write := Reg xor 16#80#;

      SPI_Begin_Transaction (SPI_2MHz);
      Clear (NCS_PIN);

      delay until Clock + Microseconds (50);

      Data_Tx (1) := Reg_Write;
      SPI_Exchange (1, Data_Tx, Data_Rx);
      --  Reg := Data_Rx (1);

      delay until Clock + Microseconds (500);

      Data_Tx (1) := Value;
      SPI_Exchange (1, Data_Tx, Data_Rx);
      --  Value := Data_Rx (1);

      delay until Clock + Microseconds (50);

      Set (NCS_PIN);
      SPI_End_Transaction;
      delay until Clock + Microseconds (200);

   end Register_Write;


   ---------------------
   --  Register_Read  --
   ---------------------
   function Register_Read (Reg : T_Uint8) return T_Uint8 is
      Data_Tx  : SPI_DMA_Data;
      Data_Rx  : SPI_DMA_Data;
      Reg_Read : T_Uint8;
   begin
      --  Set MSB to 0 for read
      Reg_Read := Reg and not 16#80#;

      SPI_Begin_Transaction (SPI_2MHz);
      Clear (NCS_PIN);

      delay until Clock + Microseconds (50);

      Data_Tx (1) := Reg_Read;
      SPI_Exchange (1, Data_Tx, Data_Rx);
      --  Reg := Data_Rx (1);

      delay until Clock + Microseconds (500);

      Data_Tx (1) := 0;
      Data_Rx (1) := 0;
      SPI_Exchange (1, Data_Tx, Data_Rx);

      delay until Clock + Microseconds (50);

      Set (NCS_PIN);
      SPI_End_Transaction;
      delay until Clock + Microseconds (200);

      return Data_Rx(1);

   end Register_Read;


   -------------------
   --  Read_motion  --
   -------------------
   procedure Read_Motion (Motion : access Motion_Burst) is
      MB_Address   : constant T_Uint8 := 16#16#;
      Data_Tx      : SPI_DMA_Data;
      Data_Rx      : SPI_DMA_Data;
      Real_Shutter : T_Uint16;

      --function Motion_Burst_Raw_To_SPI_DMA_Data is new
      --  Ada.Unchecked_Conversion (Motion_Burst_Raw, SPI_DMA_Data);

   begin
      SPI_Begin_Transaction (SPI_2MHz);
      Clear (NCS_PIN);

      delay until Clock + Microseconds (50);

      Data_Tx (1) := MB_Address;
      SPI_Exchange (1, Data_Tx, Data_Rx);

      delay until Clock + Microseconds (50);

      Data_Tx (1 .. RAW_SIZE) := T_Uint8_Array (Motion.Raw);
      SPI_Exchange (RAW_SIZE, Data_Tx, Data_Rx);
      Motion.Raw := Motion_Burst_Raw (Data_Rx (1 .. RAW_SIZE));

      delay until Clock + Microseconds (50);

      Set (NCS_PIN);
      SPI_End_Transaction;
      delay until Clock + Microseconds (50);

      Real_Shutter := Shift_Right (Motion.Shutter_1, 8) and 16#0FF#;
      Real_Shutter := Real_Shutter or Shift_Left (Motion.Shutter_1 and 16#0FF#, 8);
      Motion.Shutter_1 := Real_Shutter;

   end Read_Motion;


   ----------------------
   --  Init_Registers  --
   ----------------------
   procedure Init_Registers is
   begin
      Register_Write (16#7F#, 16#00#);
      Register_Write (16#61#, 16#AD#);
      Register_Write (16#7F#, 16#03#);
      Register_Write (16#40#, 16#00#);
      Register_Write (16#7F#, 16#05#);
      Register_Write (16#41#, 16#B3#);
      Register_Write (16#43#, 16#F1#);
      Register_Write (16#45#, 16#14#);
      Register_Write (16#5B#, 16#32#);
      Register_Write (16#5F#, 16#34#);
      Register_Write (16#7B#, 16#08#);
      Register_Write (16#7F#, 16#06#);
      Register_Write (16#44#, 16#1B#);
      Register_Write (16#40#, 16#BF#);
      Register_Write (16#4E#, 16#3F#);
      Register_Write (16#7F#, 16#08#);
      Register_Write (16#65#, 16#20#);
      Register_Write (16#6A#, 16#18#);
      Register_Write (16#7F#, 16#09#);
      Register_Write (16#4F#, 16#AF#);
      Register_Write (16#5F#, 16#40#);
      Register_Write (16#48#, 16#80#);
      Register_Write (16#49#, 16#80#);
      Register_Write (16#57#, 16#77#);
      Register_Write (16#60#, 16#78#);
      Register_Write (16#61#, 16#78#);
      Register_Write (16#62#, 16#08#);
      Register_Write (16#63#, 16#50#);
      Register_Write (16#7F#, 16#0A#);
      Register_Write (16#45#, 16#60#);
      Register_Write (16#7F#, 16#00#);
      Register_Write (16#4D#, 16#11#);
      Register_Write (16#55#, 16#80#);
      Register_Write (16#74#, 16#1F#);
      Register_Write (16#75#, 16#1F#);
      Register_Write (16#4A#, 16#78#);
      Register_Write (16#4B#, 16#78#);
      Register_Write (16#44#, 16#08#);
      Register_Write (16#45#, 16#50#);
      Register_Write (16#64#, 16#FF#);
      Register_Write (16#65#, 16#1F#);
      Register_Write (16#7F#, 16#14#);
      Register_Write (16#65#, 16#67#);
      Register_Write (16#66#, 16#08#);
      Register_Write (16#63#, 16#70#);
      Register_Write (16#7F#, 16#15#);
      Register_Write (16#48#, 16#48#);
      Register_Write (16#7F#, 16#07#);
      Register_Write (16#41#, 16#0D#);
      Register_Write (16#43#, 16#14#);
      Register_Write (16#4B#, 16#0E#);
      Register_Write (16#45#, 16#0F#);
      Register_Write (16#44#, 16#42#);
      Register_Write (16#4C#, 16#80#);
      Register_Write (16#7F#, 16#10#);
      Register_Write (16#5B#, 16#02#);
      Register_Write (16#7F#, 16#07#);
      Register_Write (16#40#, 16#41#);
      Register_Write (16#70#, 16#00#);

      delay until Clock + Milliseconds (10);

      Register_Write (16#32#, 16#44#);
      Register_Write (16#7F#, 16#07#);
      Register_Write (16#40#, 16#40#);
      Register_Write (16#7F#, 16#06#);
      Register_Write (16#62#, 16#F0#);
      Register_Write (16#63#, 16#00#);
      Register_Write (16#7F#, 16#0D#);
      Register_Write (16#48#, 16#C0#);
      Register_Write (16#6F#, 16#D5#);
      Register_Write (16#7F#, 16#00#);
      Register_Write (16#5B#, 16#A0#);
      Register_Write (16#4E#, 16#A8#);
      Register_Write (16#5A#, 16#50#);
      Register_Write (16#40#, 16#80#);

      Register_Write (16#7F#, 16#00#);
      Register_Write (16#5A#, 16#10#);
      Register_Write (16#54#, 16#00#);

   end Init_Registers;


   ---------------------
   --  Z_Ranger_Init  --
   ---------------------
   procedure Z_Ranger_Init is
      Status : Boolean := False;
   begin
      if Z_Ranger_Is_Init then
         return;
      end if;

      -- pre-compute constant in the measurement noise model for kalman
      exp_Coeff :=
        Ada.Numerics.Elementary_Functions.Log (exp_Std_B / exp_Std_A)
        / (exp_Point_B - exp_Point_A);

      --  Initialize & Configure I2C
      Initialize_I2C_GPIO (STM32.I2C.I2C_Port (Z_Ranger_Device.Port.all));
      Configure_I2C (STM32.I2C.I2C_Port (Z_Ranger_Device.Port.all));

      --  Init Z_Ranger device
      --  Set_Device_Address (Z_Ranger_Device, 16#52#, Status);

      --  Data init
      Data_Init (Z_Ranger_Device, Status);
      if not Status then return; end if;

      --  Static init
      Static_Init (Z_Ranger_Device, New_Sample_Ready, Status);
      if not Status then return; end if;

      --  Perform ref calibration
      Perform_Ref_Calibration (Z_Ranger_Device, Status);
      if not Status then return; end if;

      Set_Vcsel_Pulse_Period_Pre_Range (Z_Ranger_Device, 18, Status);
      if not Status then return; end if;
      Set_Vcsel_Pulse_Period_Final_Range (Z_Ranger_Device, 14, Status);
      --  Start_Continuous_Ranging_Measurements (Z_Ranger_Device, Status);
        --  Back to back mode

      --  Set flag
      Z_Ranger_Is_Init := Status;
   end Z_Ranger_Init;


   ---------------------
   --  Z_Ranger_Test  --
   ---------------------
   function Z_ranger_Test return Boolean is
   begin
      return Z_ranger_Is_Init;
   end Z_ranger_Test;


   ---------------------
   --  PaMotion_Init  --
   ---------------------
   procedure PaMotion_Init is
      Chip_ID     : T_Uint8;
      Inv_Chip_ID : T_Uint8;
      Set_Done    : Boolean := False;
      Dummy       : T_Uint8;
   begin
      if PaMotion_Is_Init then
         return;
      end if;

      --  Initialize CS Pin
      Set_Done := Set_Mode (NCS_PIN, HAL.GPIO.Output);
      Set (NCS_PIN);

      SPI_Begin;
      delay until Clock + Milliseconds (40);

      Set (NCS_PIN);
      delay until Clock + Milliseconds (2);
      Clear (NCS_PIN);
      delay until Clock + Milliseconds (2);
      Set (NCS_PIN);
      delay until Clock + Milliseconds (2);

      Chip_ID     := Register_Read (0);
      Inv_Chip_ID := Register_Read (16#5F#);

      --  Power on reset
      Register_Write (16#3A#, 16#5A#);
      delay until Clock + Milliseconds (5);

      --  Reading the motion registers one time
      Dummy := Register_Read (16#02#);
      Dummy := Register_Read (16#03#);
      Dummy := Register_Read (16#04#);
      Dummy := Register_Read (16#05#);
      Dummy := Register_Read (16#06#);
      delay until Clock + Milliseconds (1);

      Init_Registers;

      PaMotion_Is_Init := True;

   end PaMotion_Init;


   ---------------------
   --  PaMotion_Test  --
   ---------------------
   function PaMotion_Test return Boolean is
   begin
      return Pamotion_Is_Init;
   end PaMotion_Test;


   ---------------------
   --  Flow_Deck_Init  --
   ---------------------
   procedure Flow_Deck_Init is
   begin
      if Is_Init then
         return;
      end if;

      Z_Ranger_Init;
      PaMotion_Init;

      Is_Init := Z_Ranger_Is_Init and PaMotion_Is_Init;
   end Flow_Deck_Init;


   ---------------------
   --  Flow_Deck_Test  --
   ---------------------
   function Flow_Deck_Test return Boolean is
   begin
      return Is_Init;
   end Flow_Deck_Test;


   ---------------------
   --  Z_Ranger_Task  --
   ---------------------
   procedure Z_Ranger_Task (TOF_Data : access TOF_Measurement) is
      --  Start  : Time;
      --  Budget : Integer;
   begin
      if not Z_Ranger_Is_Init then
         return;
      end if;

      --  Start := Clock;

      --  Read height measurement ([mm])
      Last_Range :=
        T_Uint16 (Read_Range_Single_Millimeters (Z_Ranger_Device));

      --  Check if the range is feasible and update Tof Measurement
      --  the sensor shoudl not be able to measure >3 [m], and outliers
      --  typically occur at >8 [m] measurements
      if Last_Range < RANGE_OUTLIER_LIMIT then
         --  TOF_Data.Timestamp := Clock;
         TOF_Data.Distance  := 0.001 * Float (Last_Range); --  [mm] to [m]
         TOF_Data.std_Dev   :=
           exp_Std_A *
             (1.0 + Exp (exp_Coeff * (TOF_Data.Distance - exp_Point_A)));
         -- estimatorKalmanEnqueueTOF (TOF_Data);
      end if;

      --  Time allowed for one measurement (the higer, the more precise)
      --  By default, ~33 ms
      --  Budget := Integer (Measurement_Timing_Budget (Z_Ranger_Device));
      --  delay until Start + Microseconds (Budget);

   end Z_Ranger_Task;


   ---------------------
   --  PaMotion_Task  --
   ---------------------
   procedure PaMotion_Task (Flow_Data : access Flow_Measurement) is
      AccpX : T_Int16;
      AccpY : T_Int16;
   begin
      if not PaMotion_Is_Init then
         return;
      end if;

      --  delay until Clock + Milliseconds (10);

      Read_Motion (Current_Motion);

      --  Flip motion information to comply with sensor mounting
      --  (might need to be changed if mounted differently)
      AccpX := - Current_Motion.Delta_X_1;
      AccpY := - Current_Motion.Delta_Y_1;

      --  Outlier removal
      if (abs AccpX < OUTLIER_LIMIT) and (abs AccpY < OUTLIER_LIMIT)
      then
         --  Form flow measurement struct and push into the EKF


         if USE_MA_SMOOTHING then
            declare
               Mean_X : Float;
               Mean_Y : Float;
            begin
               Pixel_Averages.Average_X (Pixel_Averages.ptr) := Float (AccpX);
               Pixel_Averages.Average_Y (Pixel_Averages.ptr) := Float (AccpY);

               Mean_X := Mean_Float (Pixel_Averages.Average_X);
               Mean_Y := Mean_Float (Pixel_Averages.Average_Y);

               Pixel_Averages.ptr :=
                 (Pixel_Averages.ptr + 1) mod AVERAGE_HISTORY_LENGTH;

               Flow_Data.dpixel_X := Mean_X;
               Flow_Data.dpixel_Y := Mean_Y;
            end;

         elsif USE_LP_FILTER then
            Flow_Data.dpixel_X :=
               LP_CONSTANT * dpixel_X_Previous + (1.0 - LP_CONSTANT) * Float (AccpX);
            Flow_Data.dpixel_Y :=
               LP_CONSTANT * dpixel_Y_Previous + (1.0 - LP_CONSTANT) * Float (AccpY);

            dpixel_X_Previous := Flow_Data.dpixel_X;
            dpixel_Y_Previous := Flow_Data.dpixel_Y;

         else
            Flow_Data.dpixel_X := Float (AccpX);
            Flow_Data.dpixel_Y := Float (AccpY);
         end if;

         if not Use_Flow_Disabled then
            null; -- estimatorKalmanEnqueueFlow (Flow_Data);
         end if;

      else
         Outlier_Count := Outlier_Count + 1;
      end if;

   end PaMotion_Task;



   ----------------------------------------------------------------------------
   --  Should Motion_Burst be a record of aliased elements to make sure they
   --  have an address ?
   --  Not tested yet

   -------------------------
   --  Add_Motion_To_LOG  --
   -------------------------
   function Add_Motion_To_LOG return Boolean is
      Has_Succeed   : Boolean;
      Success       : Boolean;
      Motion_Log_ID : Natural;
   begin
      if not Log_Test then
         return False;
      end if;

      Create_Log_Group ("Motion", Motion_Log_ID, Has_Succeed);

      if Has_Succeed then
         Append_Log_Variable_To_Group (Motion_Log_ID,
                                       "Motion",
                                       LOG_UINT8,
                                       Current_Motion.Motion'Address,
                                       Has_Succeed);
         Success := Has_Succeed;

         Append_Log_Variable_To_Group (Motion_Log_ID,
                                       "Delta_X",
                                       LOG_UINT16,
                                       Current_Motion.Delta_X_2'Address,
                                       Has_Succeed);
         Success := Success and Has_Succeed;

         Append_Log_Variable_To_Group (Motion_Log_ID,
                                       "Delta_Y",
                                       LOG_UINT16,
                                       Current_Motion.Delta_Y_2'Address,
                                       Has_Succeed);
         Success := Success and Has_Succeed;

         Append_Log_Variable_To_Group (Motion_Log_ID,
                                       "Shutter",
                                       LOG_UINT16,
                                       Current_Motion.Shutter_2'Address,
                                       Has_Succeed);
         Success := Success and Has_Succeed;

         Append_Log_Variable_To_Group (Motion_Log_ID,
                                       "Max_Raw",
                                       LOG_UINT8,
                                       Current_Motion.Max_Raw_Data_2'Address,
                                       Has_Succeed);
         Success := Success and Has_Succeed;

         Append_Log_Variable_To_Group (Motion_Log_ID,
                                       "Min_Raw",
                                       LOG_UINT8,
                                       Current_Motion.Min_Raw_Data_2'Address,
                                       Has_Succeed);
         Success := Success and Has_Succeed;

         Append_Log_Variable_To_Group (Motion_Log_ID,
                                       "Raw_Sum",
                                       LOG_UINT8,
                                       Current_Motion.Raw_Data_Sum_2'Address,
                                       Has_Succeed);
         Success := Success and Has_Succeed;

         Append_Log_Variable_To_Group (Motion_Log_ID,
                                       "Outlier_Count",
                                       LOG_UINT8,
                                       Outlier_Count'Address,
                                       Has_Succeed);
         Success := Success and Has_Succeed;

         return Success;
      else
         return False;
      end if;
   end Add_Motion_To_LOG;


   -------------------------------
   --  Add_Motion_To_Parameter  --
   -------------------------------
   function Add_Motion_To_Parameter return Boolean is
       Has_Succeed     : Boolean;
       Flow_Param_Type : Parameter_Variable_Type;
       Motion_Param_ID : Natural;
   begin
      if not Parameter_Test then
         return False;
      end if;

      Flow_Param_Type.Size      := One_Byte;
      Flow_Param_Type.Floating  := False;
      Flow_Param_Type.Signed    := False;
      Flow_Param_Type.Read_Only := False;

      Create_Parameter_Group ("Motion", Motion_Param_ID, Has_Succeed);

      if Has_Succeed then
         Append_Parameter_Variable_To_Group (Motion_Param_ID,
                                             "Disable_Flow",
                                             Flow_Param_Type,
                                             Use_Flow_Disabled'Address,
                                             Has_Succeed);
         return Has_Succeed;
      else
         return False;
      end if;
   end Add_Motion_To_Parameter;


end Flow_Deck;
