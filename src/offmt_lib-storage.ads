with Ada.Strings.Unbounded;

package Offmt_Lib.Storage is

   type Store_Result (Success : Boolean) is record
      case Success is
         when True => null;
         when False =>
            Msg : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   function Store (Map : Trace_Map; Filename : String) return Store_Result;

   type Load_Result (Success : Boolean) is record
      case Success is
         when True =>
            Map : Trace_Map;
         when False =>
            Msg : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   function Load (Filename : String) return Load_Result;

end Offmt_Lib.Storage;
