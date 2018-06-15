--  Notes : in RM0090 section 10-3-3 we can find the mapping table for the DMA
--  it says for example that DMA1 can be connected to the transmitter of USART2
--  (USART2_Tx) using Channel 4 / Stream 6. This is the only right config
--  Main  mappings:
--  DMA1 : USART2_Tx / Channel 4 / Stream 6
--  DMA1 : USART2_Rx / Channel 5 / Stream 5
--  DMA2 : USART6_Tx / Channel 5 / Stream 6 or Stream 7
--  DMA2 : USART6_Rx / Chennel 5 / Stream 1 or Stream 2
--
--  Then the alternate functions map also to specific USARTs:
--  AF7 maps to USART 1..3
--  AF8 maps to USART 4..6
--
--  And in the Datasheet of STM32F405/7 we have the GPIO / AF mapping:
--  PC6 = USART6_Tx
--  PC7 = USART6_Rx
--  PA2 = USART2_Tx (AF7)
--  PA3 = USART2_Rx (AF7)

package EXT_UART is

   -------------------------------
   -- Initialize_Hardware --
   -------------------------------
   procedure Initialize_EXT_UART;

   --  ----------------------------------------------------  --
   --  Provided interface "Get_Message"
   --  ----------------------------------------------------  --
   procedure Get_Message(Last_Message: access String);

   ---------------------------------------------------------
   --  Provided interface "Send_to_UART"
   ---------------------------------------------------------
   procedure Send_to_UART(msg: String);

private
   Is_Init : Boolean := False;

end EXT_UART;
