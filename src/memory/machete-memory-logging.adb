with Machete.Images;
with Machete.Logging;

package body Machete.Memory.Logging is

   ---------------
   -- Log_Cells --
   ---------------

   procedure Log_Cells
     (Memory : Machete_Memory'Class;
      Start  : Machete_Address;
      Finish : Machete_Address)
   is
      Addr : Machete_Address := Start;
   begin
      while Addr < Finish loop
         if Addr in Stack_Address'Range then
            Machete.Logging.Log
              (Hex_Image (Addr) & ": "
               & Machete.Images.Address_Image
                 (Memory.Get_Address (Addr)));
         else
            declare
               Cell : constant Machete.Cells.Cell_Type :=
                        Memory.Get_Cell (Addr);
            begin
               if not Machete.Cells.Is_Undefined (Cell) then
                  Machete.Logging.Log
                    (Hex_Image (Addr) & ": " & Machete.Cells.Image (Cell));
               else
                  exit;
               end if;
            end;
         end if;
         Addr := Addr + Word_Size;
      end loop;
   end Log_Cells;

end Machete.Memory.Logging;
