with Machete.Memory.Logging;

package body Machete.Machine.Logging is

   ---------------
   -- Log_State --
   ---------------

   procedure Log_State
     (Machine : Machete.Machine.Machete_Machine'Class)
   is
   begin
      Machete.Memory.Logging.Log_Cells
        (Machine.Memory,
         Machine.Get_Register_Address (1),
         Machine.Get_Register_Address (63));
      Machete.Memory.Logging.Log_Cells
        (Machine.Memory, Heap_Address'First, Machine.Rs (H));
   end Log_State;

end Machete.Machine.Logging;
