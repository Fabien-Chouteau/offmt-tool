with Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;

with Interfaces;

private with System.Storage_Elements;
private with Ada.Containers.Vectors;

package Offmt_Lib is

   Log_Root_Pkg : constant Wide_Wide_String := "Offmt";

   type Format_Kind is (Decimal, Hexadecimal, Binary);
   type Format_Type is (Type_U8, Type_U16, Type_U32);

   type Format is record
      Expression : Ada.Strings.Unbounded.Unbounded_String;
      Kind : Format_Kind;
      Typ  : Format_Type;
   end record;

   type Trace_Element_Kind is (Plain_String, Format_String);

   type Trace_Element (Kind : Trace_Element_Kind) is record
      case Kind is
         when Plain_String =>
            Str : Ada.Strings.Unbounded.Unbounded_String;
         when Format_String =>
            Fmt : Format;
      end case;
   end record;

   package Trace_Element_Lists
   is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Trace_Element);

   type Trace_ID is new Interfaces.Unsigned_16;

   type Trace is record
      Original : Ada.Strings.Unbounded.Unbounded_String;
      Id       : Trace_ID;
      List     : Trace_Element_Lists.List;
   end record;

   procedure Pretty_Print (T : Trace);

   function ID_Hashed (Id : Trace_ID) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Id));

   package Trace_Maps
   is new Ada.Containers.Indefinite_Hashed_Maps (Trace_ID,
                                                 Trace,
                                                 ID_Hashed,
                                                 Equivalent_Keys => "=");

   type Trace_Map is new Trace_Maps.Map with null record;

   Empty_Map : constant Trace_Map;

   type Data_Frame is private;

private

   Empty_Map : constant Trace_Map := (Trace_Maps.Empty_Map with null record);

   package SE_Vectors
   is new Ada.Containers.Vectors (Natural,
                                  System.Storage_Elements.Storage_Element,
                                  System.Storage_Elements."=");

   type Data_Frame is record
      Data : SE_Vectors.Vector;
      Next : Natural := 0;
   end record;

   function Remaining (Frame : Data_Frame) return Natural;

   procedure Clear (Frame : in out Data_Frame)
     with Post => Remaining (Frame) = 0;

   procedure Push (Frame : in out Data_Frame;
                   Elt   :        System.Storage_Elements.Storage_Element);

   function Pop (Frame : in out Data_Frame)
                 return System.Storage_Elements.Storage_Element
     with Pre => Remaining (Frame) > 0;

   Frame_Error : exception;

end Offmt_Lib;
