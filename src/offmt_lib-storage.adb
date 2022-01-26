with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;

with Ada.Text_IO; use Ada.Text_IO;

with TOML; use TOML;
with TOML.File_IO;

package body Offmt_Lib.Storage is

   From_TOML_Error : exception;

   function To_TOML (List : Trace_Element_Lists.List) return TOML_Value;
   function To_TOML (Elt : Trace_Element) return TOML_Value;
   function To_TOML (T : Trace) return TOML_Value;

   function From_TOML (Val : TOML_Value) return Trace_Map;
   function From_TOML (Val : TOML_Value) return Trace_Element;
   function From_TOML (Val : TOML_Value) return Trace;
   function From_TOML (Val : TOML_Value) return Trace_Element_Lists.List;

   -------------
   -- To_TOML --
   -------------

   function To_TOML (Elt : Trace_Element) return TOML_Value is
   begin
      case Elt.Kind is
         when Plain_String =>
            return Create_String (Elt.Str);
         when Format_String =>
            declare
               Table : constant TOML_Value := Create_Table;
            begin
               Table.Set ("kind", Create_String (Elt.Fmt.Kind'Img));
               Table.Set ("type", Create_String (Elt.Fmt.Typ'Img));
               Table.Set ("expr", Create_String (Elt.Fmt.Expression));
               return Table;
            end;
      end case;
   end To_TOML;

   -------------
   -- To_TOML --
   -------------

   function To_TOML (List : Trace_Element_Lists.List) return TOML_Value is
      Arr : constant TOML_Value := Create_Array;
   begin
      for Elt of List loop
         Arr.Append (To_TOML (Elt));
      end loop;
      return Arr;
   end To_TOML;

   -------------
   -- To_TOML --
   -------------

   function To_TOML (T : Trace) return TOML_Value is
      Table : constant TOML_Value := Create_Table;
   begin
      Table.Set ("original", Create_String (T.Original));
      Table.Set ("format_list", To_TOML (T.List));
      return Table;
   end To_TOML;

   -----------
   -- Store --
   -----------

   function Store (Map : Trace_Map; Filename : String) return Store_Result is

      Table : constant TOML_Value := Create_Table;
      File_Out : File_Type;
   begin

      Ada.Text_IO.Create (File_Out, Out_File, Filename);

      if not Is_Open (File_Out) then
         return (Success => False,
                 Msg     => To_Unbounded_String ("Cannot open '" &
                     Filename & "' for output"));
      end if;

      for T of Map loop
         Table.Set (T.Id'Img, To_TOML (T));
      end loop;

      TOML.File_IO.Dump_To_File (Table, File_Out);

      Ada.Text_IO.Close (File_Out);

      return (Success => True);
   end Store;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (Val : TOML_Value) return Trace_Element is
   begin
      if Val.Kind = TOML_String then
         return (Kind => Plain_String, Str => Val.As_Unbounded_String);
      elsif Val.Kind = TOML_Table then
         declare
            Fmt : Format;
         begin
            Fmt.Kind := Format_Kind'Value (Val.Get ("kind").As_String);
            Fmt.Typ := Format_Type'Value (Val.Get ("type").As_String);
            Fmt.Expression := Val.Get ("expr").As_Unbounded_String;
            return (Kind => Format_String, Fmt => Fmt);
         end;
      else
         raise From_TOML_Error
           with "Invalid TOML_Kind for Trace_Element: " & Val.Kind'Img;
      end if;
   end From_TOML;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (Val : TOML_Value) return Trace_Element_Lists.List is
      Result : Trace_Element_Lists.List;
   begin
      if Val.Kind /= TOML_Array then
         raise From_TOML_Error with
           "Invalid TOML_Kind for Trace_Element_List: " & Val.Kind'Img;
      else
         for Index in 1 .. Val.Length loop
            Result.Append (From_TOML (Val.Item (Index)));
         end loop;
      end if;
      return Result;
   end From_TOML;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (Val : TOML_Value) return Trace is
      Result : Trace;
   begin
      if Val.Kind /= TOML_Table then
         raise From_TOML_Error with
           "Invalid TOML_Kind for Trace: " & Val.Kind'Img;
      else
         Result.Original := Val.Get ("original").As_Unbounded_String;
         Result.List := From_TOML (Val.Get ("format_list"));
      end if;
      return Result;
   end From_TOML;

   ---------------
   -- From_TOML --
   ---------------

   function From_TOML (Val : TOML_Value) return Trace_Map is
      Result : Trace_Map;
   begin
      if Val.Kind /= TOML_Table then
         raise From_TOML_Error with
           "Invalid TOML_Kind for Trace_Map: " & Val.Kind'Img;
      else
         for Elt of Val.Iterate_On_Table loop
            declare
               Id : constant Trace_ID := Trace_ID'Value (To_String (Elt.Key));
               T  : Trace := From_TOML (Elt.Value);
            begin
               T.Id := Id;
               Result.Include (Id, T);
            end;
         end loop;
      end if;

      return Result;
   end From_TOML;

   ----------
   -- Load --
   ----------

   function Load (Filename : String) return Load_Result is
      TOML_Result : constant TOML.Read_Result :=
        TOML.File_IO.Load_File (Filename);
   begin
      if TOML_Result.Success then
         declare
            Map : constant Trace_Map := From_TOML (TOML_Result.Value);
         begin
            return (Success => True, Map => Map);
         exception
            when E : From_TOML_Error =>
               return (Success => False,
                       Msg     => To_Unbounded_String
                         (Ada.Exceptions.Exception_Message (E)));
         end;

      else
         return (Success => False, Msg => TOML_Result.Message);
      end if;
   end Load;

end Offmt_Lib.Storage;
