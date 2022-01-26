with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

with Langkit_Support.Text;

package body Offmt_Lib.Detection is

   --------------------------
   -- Check_Defmt_Log_Call --
   --------------------------

   function Check_Defmt_Log_Call (Node : LAL.Call_Stmt'Class;
                                  Call : out LAL.Call_Expr)
                                  return Boolean
   is
   begin

      Put_Line ("Node.Image: " & Node.Image);
      Put_Line ("Node.Children_Count: " & Node.Children_Count'Img);

      if Node.Children_Count /= 1 then
         return False;
      end if;

      Call := Node.Child (1).As_Call_Expr;

      declare
         S               : constant LAL.Ada_Node := Call.F_Suffix;
         Designated_Type : constant LAL.Base_Type_Decl :=
           Call.F_Name.P_Name_Designated_Type;
      begin
         if not Designated_Type.Is_Null
           or else not Call.P_Is_Direct_Call
           or else S.Is_Null
           or else S.Children_Count /= 1
         then
            return False;
         end if;

         declare
            use Langkit_Support.Text;

            Spec : constant LAL.Subp_Spec :=
              Call.P_Called_Subp_Spec.As_Subp_Spec;

            Decl : constant LAL.Basic_Decl := Spec.P_Parent_Basic_Decl;
            Txt : constant Text_Type := Decl.P_Fully_Qualified_Name;
            Str : constant String := Image (Txt);
         begin
            Put_Line ("Fully qualified name: '" & Str & "'");
            return Str = To_String (Offmt_Lib.Log_Root_Pkg) & ".Log";
         end;
      end;
   end Check_Defmt_Log_Call;

end Offmt_Lib.Detection;
