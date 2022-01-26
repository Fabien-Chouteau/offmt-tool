with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

with Langkit_Support.Text;

package body Offmt_Lib.Rewrite is
   pragma Style_Checks ("M120");

   function U2WW (U : Unbounded_String) return Wide_Wide_String
   is (To_Wide_Wide_String (To_String (U)));

   function Create_Start_Frame (RH : LALRW.Rewriting_Handle;
                                Id : Trace_ID)
                                return LALRW.Node_Rewriting_Handle;

   function Create_Push (RH  : LALRW.Rewriting_Handle;
                         Fmt : Format)
                         return LALRW.Node_Rewriting_Handle;

   ------------------------
   -- Create_Start_Frame --
   ------------------------

   function Create_Start_Frame (RH : LALRW.Rewriting_Handle;
                                Id : Trace_ID)
                                return LALRW.Node_Rewriting_Handle
   is
   begin
      return LALRW.Create_Call_Stmt
        (RH,
         LALRW.Create_Call_Expr
           (RH,
            LALRW.Create_Dotted_Name
              (RH,
               LALRW.Create_Token_Node (RH, LALCO.Ada_Identifier, Log_Root_Pkg),
               LALRW.Create_Token_Node (RH, LALCO.Ada_Identifier, "Start_Frame")),
            LALRW.Create_Token_Node (RH, LALCO.Ada_Int_Literal, To_Wide_Wide_String (Id'Img))));
   end Create_Start_Frame;

   -----------------
   -- Create_Push --
   -----------------

   function Create_Push (RH  : LALRW.Rewriting_Handle;
                         Fmt : Format)
                         return LALRW.Node_Rewriting_Handle
   is
      Type_Str : constant String := (case Fmt.Typ is
                                        when Type_U8 => "U8",
                                        when Type_U16 => "U16",
                                        when Type_U32 => "U32");
      Proc_Str : constant String := "Push_" & Type_Str;

      --  Make an Ada expression
      Expr : constant LALRW.Node_Rewriting_Handle :=
        LALRW.Create_From_Template
          (Handle    => RH,
           Template  => U2WW (Fmt.Expression),
           Arguments => (1 .. 0 => <>),
           Rule      => LALCO.Expr_Rule);

      --  Convert it to the target type
      Convert : constant LALRW.Node_Rewriting_Handle :=
        LALRW.Create_Call_Expr
          (RH,
           LALRW.Create_Dotted_Name
             (RH,
              LALRW.Create_Token_Node (RH, LALCO.Ada_Identifier, Offmt_Lib.Log_Root_Pkg),
              LALRW.Create_Token_Node (RH, LALCO.Ada_Identifier, To_Wide_Wide_String (Type_Str))),
           Expr);

      --  Call the push procedure
      Push : constant LALRW.Node_Rewriting_Handle :=
        LALRW.Create_Call_Stmt
          (RH,
           LALRW.Create_Call_Expr
             (RH,
              LALRW.Create_Dotted_Name
                (RH,
                 LALRW.Create_Token_Node (RH, LALCO.Ada_Identifier, Offmt_Lib.Log_Root_Pkg),
                 LALRW.Create_Token_Node (RH, LALCO.Ada_Identifier, To_Wide_Wide_String (Proc_Str))),
              Convert));

   begin
      return Push;
   end Create_Push;

   ----------------------
   -- Rewrite_Log_Call --
   ----------------------

   procedure Rewrite_Log_Call (RH   : LALRW.Rewriting_Handle;
                               Node : LAL.Call_Stmt'Class;
                               T    : Trace)
   is
      Stmt_List : constant LALRW.Node_Rewriting_Handle :=
        LALRW.Create_Node (RH, LALCO.Ada_Stmt_List);

      SH  : constant LALRW.Node_Rewriting_Handle := LALRW.Handle (Node);
   begin

      LALRW.Append_Child (Stmt_List, Create_Start_Frame (RH, T.Id));

      for Elt of T.List loop
         case Elt.Kind is
            when Plain_String => null;

            when Format_String =>
               LALRW.Append_Child (Stmt_List, Create_Push (RH, Elt.Fmt));
         end case;
      end loop;

      LALRW.Append_Child
        (Stmt_List,
         LALRW.Create_From_Template (Handle    => RH,
                                     Template  => Log_Root_Pkg & ".End_Frame;",
                                     Arguments => (1 .. 0 => <>),
                                     Rule      => LALCO.Simple_Stmt_Rule));

      declare
         use Langkit_Support.Text;

         Str : constant String := Image (LALRW.Unparse (Stmt_List));
      begin
         Put_Line ("Rewrite: '" & Str & "'");
      end;

      LALRW.Replace (SH, Stmt_List);
   end Rewrite_Log_Call;

end Offmt_Lib.Rewrite;
