-----------------------------------------
--    D E C K - S P I   D R I V E R    --
-----------------------------------------

--  Derived from deck_spi.c of the Crazyflie frimware

with STM32.Board;	   use STM32.Board;
with STM32.GPIO;	   use STM32.GPIO;
with STM32.DMA;		   use STM32.DMA;
with Ada.Interrupts;       use Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with STM32.Device;	   use STM32.Device;
with HAL.SPI;
--  with EXT_UART;          use EXT_UART;


package body STM32.SPI.DECK is

   --  EXT_SPI defined in STM32.Board, renames SPI1
   EXT_SPI_DMA : aliased SPI_Port_DMA_TX_RX (Internal_SPI_1'Access);
   EXT_SPI_AF : constant STM32.GPIO_Alternate_Function := GPIO_AF_SPI1_5;

   SPI_DMA : DMA_Controller renames DMA_2;

   SPI_TX_DMA_STREAM     : constant DMA_Stream_Selector := Stream_5;
   SPI_TX_DMA_IRQ        : constant Interrupt_ID := DMA2_Stream5_Interrupt;
   SPI_TX_DMA_IRQHandler : DMA_Interrupt_Controller renames STM32.Device.DMA2_Stream5;
   SPI_TX_DMA_CHANNEL    : constant DMA_Channel_Selector := Channel_3;

   SPI_RX_DMA_STREAM     : constant DMA_Stream_Selector := Stream_0;
   SPI_RX_DMA_IRQ        : constant Interrupt_ID := DMA2_Stream0_Interrupt;
   SPI_RX_DMA_IRQHandler : DMA_Interrupt_Controller renames STM32.Device.DMA2_Stream0;
   SPI_RX_DMA_CHANNEL    : constant DMA_Channel_Selector := Channel_3;
   
   
   DUMMY_BYTE : constant Unsigned_8 := 16#A5#;
   
   SPI_Speed_To_BRP : constant array (SPI_Speed) of SPI_Baud_Rate_Prescaler :=
     (SPI_21MHz => BRP_4,
      SPI_12MHz => BRP_8, 
      SPI_6MHz  => BRP_16, 
      SPI_3MHz  => BRP_32, 
      SPI_2MHz  => BRP_64);


   -----------------
   --  SPI_Begin  --
   -----------------
   procedure SPI_Begin is
      Config_GPIO : GPIO_Port_Configuration;
   begin
      
      --  Enable the SPI clock
      Enable_Clock (EXT_SPI);
      
      --  Enable GPIO clocks
      Enable_Clock (EXT_SCK & EXT_MISO & EXT_MOSI);
      
      --  Enable DMA clocks
      Enable_Clock (SPI_DMA);
      
      ------------------------------
      --  SPI pins configuration  --
      ------------------------------
      
      --  Connect SPI pins to AF5
      Configure_Alternate_Function (EXT_SCK & EXT_MISO & EXT_MOSI,
                                    AF => EXT_SPI_AF);
      
      Config_GPIO.Mode	      := Mode_AF;
      Config_GPIO.Speed       := Speed_50MHz;
      Config_GPIO.Output_Type := Push_Pull;
      Config_GPIO.Resistors   := Pull_Down;

      Configure_IO (EXT_SCK & EXT_MISO & EXT_MOSI,
                    Config => Config_GPIO);
      
      ------------------------------
      --  SPI DMA initialization  --
      ------------------------------
      --  Set TX/RX DMA Interrupt Controllers of SPI_Port_DMA
      EXT_SPI_DMA.TX_DMA := SPI_TX_DMA_IRQHandler'Access;
      EXT_SPI_DMA.RX_DMA := SPI_RX_DMA_IRQHandler'Access;
      --  Initialize DMA
      SPI_DMA_Init;
      
      -------------------------
      --  SPI configuration  --
      -------------------------
      SPI_Config_With_Speed (SPI_2MHz);
      
      Set_True (SPI_Access);
      Is_Init := True;
      
   end SPI_Begin;

   
   --------------------
   --  SPI_DMA_Init  --
   --------------------
   procedure SPI_DMA_Init is
      Config_DMA : DMA_Stream_Configuration;
   begin
      
      Config_DMA.Channel                      := SPI_TX_DMA_CHANNEL;
      Config_DMA.Direction                    := Memory_To_Peripheral;
      Config_DMA.Increment_Peripheral_Address := False;
      Config_DMA.Increment_Memory_Address     := True;
      Config_DMA.Peripheral_Data_Format       := Bytes;
      Config_DMA.Memory_Data_Format           := Bytes;
      Config_DMA.Operation_Mode               := Normal_Mode;
      Config_DMA.Priority                     := Priority_High;
      Config_DMA.FIFO_Enabled                 := False;
      Config_DMA.FIFO_Threshold               := FIFO_Threshold_1_Quart_Full_Configuration;
      Config_DMA.Memory_Burst_Size            := Memory_Burst_Single;
      Config_DMA.Peripheral_Burst_Size        := Peripheral_Burst_Single;

      Reset (SPI_DMA, SPI_TX_DMA_STREAM);
      Configure (SPI_DMA, SPI_TX_DMA_STREAM, Config_DMA);

      Config_DMA.Channel := SPI_RX_DMA_CHANNEL;
      Config_DMA.Direction := Peripheral_To_Memory;
      Reset (SPI_DMA, SPI_RX_DMA_STREAM);
      Configure (SPI_DMA, SPI_RX_DMA_STREAM, Config_DMA);
      
   end SPI_DMA_Init;
   

   -----------------------------
   --  SPI_Config_With_Speed  --
   -----------------------------
   procedure SPI_Config_With_Speed (Speed : SPI_Speed) is
      Config_SPI : SPI_Configuration;
   begin
      
      Config_SPI.Direction           := D2Lines_FullDuplex;
      Config_SPI.Mode                := Master;
      Config_SPI.Data_Size           := HAL.SPI.Data_Size_8b;
      Config_SPI.Clock_Polarity      := Low;
      Config_SPI.Clock_Phase         := P1Edge;
      Config_SPI.Slave_Management    := Software_Managed;
      Config_SPI.Baud_Rate_Prescaler := SPI_Speed_To_BRP (Speed);
      Config_SPI.First_Bit           := MSB;
      Config_SPI.CRC_Poly            := 0;  --  Not used
      
      Configure (EXT_SPI_DMA, Config_SPI);
      
   end SPI_Config_With_Speed;
   

   ----------------
   --  SPI_Test  --
   ----------------
   function SPI_Test 
     return Boolean is      
   begin
      return Is_Init;
   end SPI_Test;
   

   --------------------
   --  SPI_Exchange  --
   --------------------
   procedure SPI_Exchange 
     (Data_Size : Natural;
      Data_Tx   : SPI_DMA_Data;
      Data_Rx   : out SPI_DMA_Data)
   is
      Result_Tx : DMA_Error_Code;
      Result_Rx : DMA_Error_Code;
   begin
      --  Enable SPI DMA Interrupts
      Enable_Interrupt (SPI_DMA, SPI_TX_DMA_STREAM, Transfer_Complete_Interrupt);
      Enable_Interrupt (SPI_DMA, SPI_RX_DMA_STREAM, Transfer_Complete_Interrupt);
      
      --  Clear DMA Flags
      Clear_All_Status (SPI_DMA, SPI_TX_DMA_STREAM);
      Clear_All_Status (SPI_DMA, SPI_RX_DMA_STREAM);

      --  Disable Streams, Configure Data Flow and Enable Streams
      EXT_SPI_DMA.TX_DMA.Start_Transfer
        (Source      => Data_Tx'Address,
         Destination => EXT_SPI_DMA.Data_Register_Address,
         Data_Count  => UInt16 (Data_Size));
      
      EXT_SPI_DMA.RX_DMA.Start_Transfer
        (Source      => EXT_SPI_DMA.Data_Register_Address,
         Destination => Data_Rx'Address,
         Data_Count  => UInt16 (Data_Size));
      
      --  Enable SPI DMA Requests
      Enable_DMA_Transmit_Requests (EXT_SPI_DMA);
      Enable_DMA_Receive_Requests (EXT_SPI_DMA);
      
      --  Enable peripheral
      Enable (EXT_SPI);

      --  Wait for completion
      EXT_SPI_DMA.TX_DMA.Wait_For_Completion (Status => Result_Tx);
      EXT_SPI_DMA.RX_DMA.Wait_For_Completion (Status => Result_Rx);
      
      ------  Needed ?  ------
      --  Disable peripheral
      Disable (EXT_SPI);
      
      --  Disable SPI DMA Interrupts
      Disable_Interrupt (SPI_DMA, SPI_TX_DMA_STREAM, Transfer_Complete_Interrupt);
      Disable_Interrupt (SPI_DMA, SPI_RX_DMA_STREAM, Transfer_Complete_Interrupt);
      
      --  Disable SPI DMA Requests
      Disable_DMA_Transmit_Requests (EXT_SPI_DMA);
      Disable_DMA_Receive_Requests (EXT_SPI_DMA);

      case Result_Tx is
         when DMA_No_Error | DMA_FIFO_Error =>
            null;
         when others =>
            raise Program_Error with Result_Tx'Img;
      end case;
      
      case Result_Rx is
         when DMA_No_Error | DMA_FIFO_Error =>
            null;
         when others =>
            raise Program_Error with Result_Rx'Img;
      end case;
   end SPI_Exchange;
   

   -----------------------------
   --  SPI_Begin_Transaction  --
   -----------------------------
   procedure SPI_Begin_Transaction (Speed : SPI_Speed) is
   begin
      Suspend_Until_True (SPI_Access);
      SPI_Config_With_Speed (Speed);
   end SPI_Begin_Transaction;
   
   
   procedure SPI_End_Transaction is
   begin
      Set_True (SPI_Access);
   end SPI_End_Transaction;
   
   
   ----------------------------------------
   --  Enable/Disable TX/RX DMA Requests --
   ----------------------------------------
   
   procedure Enable_DMA_Transmit_Requests (This : in out SPI_Port_DMA_TX_RX) is
   begin
      This.Periph.CR2.TXDMAEN := True;
   end Enable_DMA_Transmit_Requests;
   
   procedure Enable_DMA_Receive_Requests (This : in out SPI_Port_DMA_TX_RX) is
   begin
      This.Periph.CR2.RXDMAEN := True;
   end Enable_DMA_Receive_Requests;
   
   procedure Disable_DMA_Transmit_Requests (This : in out SPI_Port_DMA_TX_RX) is
   begin
      This.Periph.CR2.TXDMAEN := False;
   end Disable_DMA_Transmit_Requests;
   
   procedure Disable_DMA_Receive_Requests (This : in out SPI_Port_DMA_TX_RX) is
   begin
      This.Periph.CR2.RXDMAEN := False;
   end Disable_DMA_Receive_Requests;
   
   ---------------
   -- Configure --
   ---------------

   overriding procedure Configure
     (This : in out SPI_Port_DMA_TX_RX;
      Conf : SPI_Configuration)
   is
   begin
      Configure (Parent (This), Conf);
   end Configure;
   

end STM32.SPI.DECK;
