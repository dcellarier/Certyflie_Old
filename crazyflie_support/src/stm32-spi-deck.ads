-----------------------------------------
--    D E C K - S P I   D R I V E R    --
-----------------------------------------

--  Derived from deck_spi of the Crazyflie frimware

with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;
with STM32.DMA.Interrupts;         use STM32.DMA.Interrupts;
with System;
with STM32.SPI;                    use STM32.SPI;
with Types;                        use Types;

package STM32.SPI.DECK is

   subtype Parent is SPI_Port;
   type SPI_Port_DMA_TX_RX is limited new Parent with private;
   
   subtype SPI_DMA_Data is T_Uint8_Array (1 .. 64);
   
   type SPI_Speed is (SPI_21MHz, SPI_12MHz, SPI_6MHz, SPI_3MHz, SPI_2MHz);

   overriding
   procedure Configure (This : in out SPI_Port_DMA_TX_RX;
                        Conf : SPI_Configuration);
   
   --  Initialize the SPI
   procedure SPI_Begin;

   function SPI_Test return Boolean;

   procedure SPI_Begin_Transaction (Speed : SPI_Speed);

   procedure SPI_End_Transaction;

   --  Send the Data_Tx buffer and receive into Data_Rx buffer
   procedure SPI_Exchange (Data_Size : Natural; 
                           Data_Tx : SPI_DMA_Data;
                           Data_Rx : out SPI_DMA_Data);

   
private
   
   --  Global variables
   Is_Init    : Boolean := False;   
   SPI_Access : Suspension_Object;

   type SPI_Port_DMA_TX_RX is limited new Parent with record
      TX_DMA  : DMA_Interrupt_Controller_Access := null;
      RX_DMA  : DMA_Interrupt_Controller_Access := null;
   end record;
   
   procedure SPI_DMA_Init;  
   
   procedure SPI_Config_With_Speed(Speed : SPI_Speed);
   
   procedure Enable_DMA_Transmit_Requests (This : in out SPI_Port_DMA_TX_RX); 
   
   procedure Enable_DMA_Receive_Requests (This : in out SPI_Port_DMA_TX_RX); 
   
   procedure Disable_DMA_Transmit_Requests (This : in out SPI_Port_DMA_TX_RX);
   
   procedure Disable_DMA_Receive_Requests (This : in out SPI_Port_DMA_TX_RX);

end STM32.SPI.DECK;
