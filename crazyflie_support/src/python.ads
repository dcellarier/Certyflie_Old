with System;
with Ada.Streams;
with POHICDRIVER_BLUETOOTH;

package Python is

   package AS renames Ada.Streams;

   Interpreter_Error : exception;
   
   type Module is private;
   
   type URI is
      record
         Radio_ID : Natural;
         Channel  : Natural range 0 .. 125;
         Datarate : POHICDRIVER_BLUETOOTH.Datarate_T;
      end record;
   
   type URIs_List is array (Natural range <>) of URI;

   procedure Initialize (Program_Name : in String := "");
   procedure Finalize;
    
   procedure Execute_String (Script : in String);

   function Import_File (File_Name : in String) return Module;
   procedure Close_Module (M : in Module);
   
   --  Overloads for "all" needed combinations of parameters and return types:
   
   protected Calls is
      procedure Call_Cflib_Init (Uris : URIs_list);
      procedure Call_Cflib_Destroy;
      procedure Call_Receive_Packet_Data (Cfid : Integer;
                                          Item : out AS.Stream_Element_Array;
                                          Last : out AS.Stream_Element_Offset);
      function Call_Send_Packet_Data (Cfid : Integer; 
                                      Msg : AS.Stream_Element_Array; 
                                      Size : AS.Stream_Element_Offset)
                                     return Boolean;
   end Calls;
   
private

   type Module is new System.Address;

end Python;

