with Interfaces.C;
with Ada.Strings;
with Ada.Strings.Fixed;
with POHICDRIVER_BLUETOOTH;
use POHICDRIVER_BLUETOOTH;
--  with Ada.Unchecked_Conversion;

package body Python is

   subtype PyObject is System.Address;

   procedure Py_SetProgramName (Name : in Interfaces.C.char_array);
   pragma Import (C, Py_SetProgramName, "Py_SetProgramName");

   procedure Py_Initialize;
   pragma Import (C, Py_Initialize, "Py_Initialize");

   procedure Py_Finalize;
   pragma Import (C, Py_Finalize, "Py_Finalize");
    
   function PyRun_SimpleString (Command : in Interfaces.C.char_array) return Interfaces.C.int;
   pragma Import (C, PyRun_SimpleString, "PyRun_SimpleString");
    
   --  procedure Py_IncRef (Obj : in PyObject);
   --  pragma Import (C, Py_IncRef, "Py_IncRef");
    
   procedure Py_DecRef (Obj : in PyObject);
   pragma Import (C, Py_DecRef, "Py_DecRef");
    
   --  function PyInt_AsLong (I : in PyObject) return Interfaces.C.long;
   --  pragma Import (C, PyInt_AsLong, "PyInt_AsLong");
      
   function PyUnicode_DecodeFSDefault (Str : in Interfaces.C.char_array) return PyObject;
   pragma Import (C, PyUnicode_DecodeFSDefault, "PyUnicode_DecodeFSDefault");
    
   function PyImport_Import (Obj : in PyObject) return PyObject;
   pragma Import (C, PyImport_Import, "PyImport_Import");
   
   function PyObject_GetAttrString (Obj : in PyObject; Name : in Interfaces.C.char_array) return PyObject;
   pragma Import (C, PyObject_GetAttrString, "PyObject_GetAttrString");
   
   function PyObject_CallObject (Obj : in PyObject; Args : in PyObject) return PyObject;
   pragma Import (C, PyObject_CallObject, "PyObject_CallObject");
   
   --  procedure PyErr_Print;
   --  pragma Import (C, PyErr_Print, "PyErr_Print");
   
   function PyCallable_Check (Obj : PyObject) return Interfaces.C.int;
   pragma Import (C, PyCallable_Check, "PyCallable_Check");
   
   function PyObject_IsTrue (Obj : PyObject) return Interfaces.C.int;
   pragma Import (C, PyObject_IsTrue, "PyObject_IsTrue");
   
   --  TUPLE  --
   function PyTuple_New (Len : Interfaces.C.int) return PyObject;
   pragma Import (C, PyTuple_New, "PyTuple_New");
         
   function PyTuple_SetItem (T : PyObject; Pos : Interfaces.C.int; O : PyObject) return Interfaces.C.int;
   pragma Import (C, PyTuple_SetItem, "PyTuple_SetItem");

   function PyTuple_Pack (N : in Interfaces.C.int; Arg1 : PyObject; Arg2 : PyObject) return PyObject;
   pragma Import (C, PyTuple_Pack, "PyTuple_Pack");
   
   --  LONG  --
   function PyLong_FromSize_t (ID : Interfaces.C.size_t) return PyObject;
   pragma Import (C, PyLong_FromSize_t, "PyLong_FromSize_t");

   function PyLong_AsSize_t (Obj : PyObject) return Interfaces.C.size_t;
   pragma Import (C, PyLong_AsSize_t, "PyLong_AsSize_t");
   
   --  LIST  --
   function PyList_New (Len : Interfaces.C.size_t) return PyObject;
   pragma Import (C, PyList_New, "PyList_New");

   function PyList_SetItem (L : PyObject; Pos : Interfaces.C.size_t; O : PyObject) return Interfaces.C.int;
   pragma Import (C, PyList_SetItem, "PyList_SetItem");

   function PyList_GetItem (Obj : PyObject; i : Interfaces.C.size_t) return PyObject;
   pragma Import (C, PyList_GetItem, "PyList_GetItem");
         
   function PyList_Size (Obj : PyObject) return Interfaces.C.size_t;
   pragma Import (C, PyList_Size, "PyList_Size");


   
   ------------------------
   --  Global variables  --
   ------------------------
   Cflib : Module;
   
   type FUNCS is (INIT, CLEANUP, SEND_PACKET_DATA, RECEIVE_PACKET_DATA);
   
   PyFunctions : array (FUNCS) of PyObject;
   
    
   --------------------------------
   --  Procedures and Functions  --
   --------------------------------
   procedure Initialize (Program_Name : in String := "") is
   begin
      if Program_Name /= "" then
         declare
            C_Name : constant Interfaces.C.char_array := Interfaces.C.To_C (Program_Name);
         begin
            Py_SetProgramName (C_Name);
         end;
      end if;
       
      Py_Initialize;
      
      --  Below: workaround for the following issue:
      --  http://stackoverflow.com/questions/13422206/how-to-load-a-custom-python-module-in-c

      Execute_String ("import sys");
      Execute_String ("sys.path.append('.')");
   end Initialize;
    
   procedure Finalize is
   begin
      Py_Finalize;
   end Finalize;
    
   procedure Execute_String (Script : in String) is
      Dummy : Interfaces.C.int;
   begin
      Dummy := PyRun_SimpleString (Interfaces.C.To_C (Script));
   end Execute_String;
    
   function Import_File (File_Name : in String) return Module is
      PyFileName : constant PyObject := PyUnicode_DecodeFSDefault (Interfaces.C.To_C (File_Name));
      M : constant PyObject := PyImport_Import (PyFileName);
      
      use type System.Address;
   begin
      Py_DecRef (PyFileName);
      if M = System.Null_Address then
         --PyErr_Print;
         raise Interpreter_Error with "Cannot load module from file " & File_Name;
      end if;
       
      return Module (M);
   end Import_File;
   
   procedure Close_Module (M : in Module) is
   begin
      Py_DecRef (PyObject (M));
   end Close_Module;

   
   ------------------------------------------------------------
   --  helpers for use from all overloaded Call subprograms  --
   ------------------------------------------------------------
   function Get_Symbol (M : in Module; Function_Name : in String) return PyObject is
      PyModule : constant PyObject := PyObject (M);
      F : constant PyObject := PyObject_GetAttrString (PyModule, Interfaces.C.To_C (Function_Name));
      use type System.Address;
   begin
      if F = System.Null_Address then
         --PyErr_Print;
         raise Interpreter_Error with "Cannot find function " & Function_Name;
      end if;
      
      if Integer (PyCallable_Check (F)) = 0 then
         raise Interpreter_Error with Function_Name & " is not callable";
      end if;
      
      return F;
   end Get_Symbol;
   
   function Call_Object (F : in PyObject; Function_Name : in String; PyParams : in PyObject) return PyObject is
      PyResult : PyObject;
      use type System.Address;
   begin
      PyResult := PyObject_CallObject (F, PyParams);
      if PyResult = System.Null_Address then
         raise Interpreter_Error with "Operation " & Function_Name & " did not return expected result";
      end if;
      
      return PyResult;
   end Call_Object;      

   
   protected body Calls is      
      
      ------------------
      --  Cflib_Init  --
      ------------------
      procedure Call_Cflib_Init (Uris : URIs_list) is
         PyUri    : PyObject := System.Null_Address;
         PyArgs   : PyObject;
         PyResult : PyObject;
         Item_Set : Integer;
         
         --  import PyUnicode_FromString, PyTuple_New, PyTuple_SetItem
         function PyUnicode_FromString (Str : Interfaces.C.char_array) return PyObject;
         pragma Import (C, PyUnicode_FromString, "PyUnicode_FromString");

         use type System.Address;
      begin
         --  Initialize python library
         Initialize;
         Cflib := Import_File ("cflibwrapper3");
         
         --  Initialize functions
         PyFunctions (INIT) := Get_Symbol (Cflib, "init");
         PyFunctions (CLEANUP) := Get_Symbol (Cflib, "cleanup");
         PyFunctions (SEND_PACKET_DATA) := Get_Symbol (Cflib, "send_packet_data");
         PyFunctions (RECEIVE_PACKET_DATA) := Get_Symbol (Cflib, "receive_packet_data");
         
         --  Initialize Crazyflies and connect
         for Uri of Uris loop
            declare
               Temp : constant String := Datarate_T'Image (Uri.Datarate);
               URI_Str : constant String := "radio://" 
                 & Ada.Strings.Fixed.Trim (Natural'Image (Uri.Radio_ID), Ada.Strings.Left)
                 & "/" & Ada.Strings.Fixed.Trim (Natural'Image (Uri.Channel), Ada.Strings.Left)
                 & "/" & Temp (2 .. Temp'Last);
            begin
               PyUri := PyUnicode_FromString (Interfaces.C.To_C (URI_Str));
            
               PyArgs := PyTuple_New (1);           
               if PyUri = System.Null_Address then
                  Py_DecRef (PyArgs);
                  raise Interpreter_Error with "Cannot convert argument : " & URI_Str;
               end if;
            
               Item_Set := Integer (PyTuple_SetItem (PyArgs, 0, PyUri));
               PyResult := Call_Object (PyFunctions (INIT), "init", PyArgs);
               if Integer (PyObject_IsTrue (PyResult)) <= 0 then
                  Py_DecRef (PyArgs);
                  Py_DecRef (PyResult);
                  raise Interpreter_Error with "Failed to initialize Crazyflie " & URI_Str;
               end if; 
            end;
         end loop;
         
         Py_DecRef (PyArgs);
         Py_Decref (PyResult);
         
      end Call_Cflib_Init;
      
      
      ---------------------
      --  Cflib_Destroy  --
      ---------------------
      procedure Call_Cflib_Destroy is
         Clean_Succeed : PyObject;
         
         use type System.Address;        
      begin
         if System.Address (Cflib) /= System.Null_Address then
            if PyFunctions (CLEANUP) /= System.Null_Address then
               Clean_Succeed := Call_Object (PyFunctions (CLEANUP), "cleanup", System.Null_Address);
            end if;
            if Integer (PyObject_IsTrue (Clean_Succeed)) <= 0 then
               raise Interpreter_Error with "Failed to destroy Cflib";
            end if; 
            Close_Module (Cflib);
            for F in FUNCS loop
               Py_DecRef (PyFunctions (F));
            end loop;
         end if;
         Finalize;
            
      end Call_Cflib_Destroy;
      

      ---------------------------
      --  Receive_Packet_Data  --
      ---------------------------
      procedure Call_Receive_Packet_Data (Cfid : Integer; 
                                          Item : out AS.Stream_Element_Array; 
                                          Last : out AS.Stream_Element_Offset)
      is
         PyArgs   : PyObject;
         PyCfid   : PyObject;
         PyResult : PyObject;
         Item_Set : Interfaces.C.int;
         Size     : Interfaces.C.size_t;
         
         use type AS.Stream_Element_Offset;
         use type Interfaces.C.size_t;
      begin
         PyCfid := PyLong_FromSize_t (Interfaces.C.size_t (Cfid));
         PyArgs := PyTuple_New (1);           
         Item_Set := PyTuple_SetItem (PyArgs, 0, PyCfid);
         PyResult := Call_Object (PyFunctions (RECEIVE_PACKET_DATA), "Receive_Packet_Data", PyArgs);

         Size := PyList_Size (PyResult);

         if Integer (Size) = 0 then
            raise Interpreter_Error with "Crazyflie ID out of range or initialization failed";
         elsif Integer (Size) = 1 then
            --  Crazyflie disconnected
            Last := Item'First - 1;   
         elsif Integer (Size) = 2 then
            --  Received empty packet or not with polyorb message
            Last := Item'First;
         else
            --  Received polyorb message
            for i in Interfaces.C.size_t (0) .. Size-1 loop
               Item (AS.Stream_Element_Offset (i+1)) 
                 := AS.Stream_Element (PyLong_AsSize_t (PyList_GetItem (PyResult, i)));
            end loop;
            Last := AS.Stream_Element_Offset (Size);
         end if;
         
         Py_DecRef (PyArgs);
         --Py_DecRef (PyCfid);
         Py_DecRef (PyResult);
         
      end Call_Receive_Packet_Data;

      
      ------------------------
      --  Send_Packet_Data  --
      ------------------------
      function Call_Send_Packet_Data (Cfid : Integer; 
                                       Msg : AS.Stream_Element_Array; 
                                       Size : AS.Stream_Element_Offset)
                                     return Boolean
      is
         PyCfid   : PyObject;
         PyMsg    : PyObject;
         PyArgs   : PyObject;
         PyResult : PyObject;
         Sizet    : constant Interfaces.C.size_t := Interfaces.C.size_t (Size);
         Item     : PyObject;
         Item_Set : Interfaces.C.int;
         Msg_Sent : Boolean;
         
         use type AS.Stream_Element_Offset;
         use type Interfaces.C.size_t;
      begin
         if Size = 0 then
            return False;
         end if;
         
         PyCfid := PyLong_FromSize_t (Interfaces.C.size_t (Cfid));
         PyMsg := PyList_New (Sizet);
         for i in 0 .. Sizet-1 loop
            Item := PyLong_FromSize_t 
                     (Interfaces.C.size_t 
                      (Msg (AS.Stream_Element_Offset (i+1))));
            Item_Set := PyList_SetItem (PyMsg, i, Item);
         end loop;
         
         PyArgs := PyTuple_Pack (2, PyCfid, PyMsg);
         PyResult := Call_Object (PyFunctions (SEND_PACKET_DATA), "Send_Packet_Data", PyArgs);
         
         if Integer (PyObject_IsTrue (PyResult)) <= 0 then
            Msg_Sent := False;
         else 
            Msg_Sent := True;
         end if;

         Py_DecRef (PyCfid);
         Py_DecRef (PyMsg);
         --Py_DecRef (Item);
         Py_DecRef (PyArgs);
         Py_DecRef (PyResult);

         return Msg_Sent;

      end Call_Send_Packet_Data;
      
   end Calls;

end Python;

