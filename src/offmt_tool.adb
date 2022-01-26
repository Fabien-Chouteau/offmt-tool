
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Helpers;
with Libadalang.Unparsing;

with GNATCOLL; use GNATCOLL;
with GNATCOLL.VFS;
with GNATCOLL.Opt_Parse;
with GNATCOLL.Traces;
with GNATCOLL.Strings;

with GNAT.OS_Lib;

with Langkit_Support.Diagnostics;

with Libadalang.Common;
with Libadalang.Rewriting;
with Libadalang.Expr_Eval;

with Offmt_Lib;
with Offmt_Lib.Parser;
with Offmt_Lib.Rewrite;
with Offmt_Lib.Detection;
with Offmt_Lib.Storage;
with Offmt_Lib.Decoding;

procedure Offmt_Tool is

   package Helpers renames Libadalang.Helpers;
   package LAL     renames Libadalang.Analysis;
   package LALU    renames Libadalang.Unparsing;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;
   package LALEXPR renames Libadalang.Expr_Eval;

   All_Traces : Offmt_Lib.Trace_Map;

   function Base_Name (Full_Path : String) return String;

   procedure Setup (Ctx   : Helpers.App_Context;
                    Jobs  : Helpers.App_Job_Context_Array);

   procedure Process_Unit (Job_Ctx : Helpers.App_Job_Context;
                           Unit : LAL.Analysis_Unit);

   procedure Post_Process (Ctx : Helpers.App_Context;
                           Jobs : Helpers.App_Job_Context_Array);

   procedure Output_Unit
     (Job_Ctx      : Libadalang.Helpers.App_Job_Context;
      Unit         : Libadalang.Analysis.Analysis_Unit;
      Out_Dir_Path : GNATCOLL.Strings.XString);

   procedure Output_Map (Map : Offmt_Lib.Trace_Map);

   function Map_Filename return String;

   procedure Handle_Log_Call (RH : LALRW.Rewriting_Handle;
                              Node : LAL.Call_Stmt'Class);

   package App is new Helpers.App
     (Name             => To_String (Offmt_Lib.Log_Root_Pkg),
      Description      => "Offloaded string format",
      App_Setup        => Setup,
      App_Post_Process => Post_Process,
      Process_Unit     => Process_Unit);

   package Output_Dir is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => App.Args.Parser,
      Long        => "--output-dir",
      Arg_Type    => GNATCOLL.Strings.XString,
      Convert     => GNATCOLL.Opt_Parse.Convert,
      Default_Val => GNATCOLL.Strings.Null_XString,
      Help        =>
         "The directory in which to output the instrumented ada units."
       & "When invoked with a project file, this path is treated as "
       & "relative to the (sub-)projects' object directories, unless "
       & "this is an absolute path.");

   Instr_Mode_Long : constant String := "--instrument";
   Decode_Mode_Long : constant String := "--decode";

   package Instrument_Mode is new GNATCOLL.Opt_Parse.Parse_Flag
     (Parser  => App.Args.Parser,
      Long    => Instr_Mode_Long,
      Help    => "Enable instrumentation mode");

   package Decode_Mode is new GNATCOLL.Opt_Parse.Parse_Flag
     (Parser  => App.Args.Parser,
      Long    => Decode_Mode_Long,
      Help    => "Enable decode mode");

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (Full_Path : String) return String is
      use GNATCOLL.VFS;
   begin
      return +Create (+Full_Path).Base_Name;
   end Base_Name;

   ------------------
   -- Map_Filename --
   ------------------

   function Map_Filename return String is

      Out_Dir : constant VFS.Virtual_File :=
        VFS.Create_From_Base (VFS."+" (Strings.To_String (Output_Dir.Get)));

      Out_File : constant VFS.Virtual_File :=
        VFS.Join (Out_Dir, "offmt_map.toml");

   begin
      return VFS."+" (Out_File.Full_Name);
   end Map_Filename;

   ---------------------
   -- Handle_Log_Call --
   ---------------------

   procedure Handle_Log_Call (RH : LALRW.Rewriting_Handle;
                              Node : LAL.Call_Stmt'Class)
   is
      Call : LAL.Call_Expr;
   begin
      if Offmt_Lib.Detection.Check_Defmt_Log_Call (Node, Call) then
         declare
            S   : constant LAL.Ada_Node := Call.F_Suffix;
            Arg : LAL.Expr;
         begin

            Arg := S.Child (1).As_Param_Assoc.F_R_Expr;
            declare
               Arg_Val : constant LALEXPR.Eval_Result :=
                 LALEXPR.Expr_Eval (Arg);
            begin
               case Arg_Val.Kind is
               when LALEXPR.String_Lit =>
                  declare
                     use Langkit_Support.Text;
                     Txt : constant Text_Type
                       := To_Text (Arg_Val.String_Result);
                     Str : constant String := Image (Txt);
                     Trace : constant Offmt_Lib.Trace :=
                       Offmt_Lib.Parser.Parse (Str);
                  begin
                     Put_Line (Str);
                     Offmt_Lib.Pretty_Print (Trace);
                     Offmt_Lib.Rewrite.Rewrite_Log_Call (RH, Node, Trace);
                     All_Traces.Include (Trace.Id, Trace);
                  end;

               when others =>
                  raise Program_Error with "String lit expected";
               end case;
            end;
         end;
      end if;
   end Handle_Log_Call;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (Job_Ctx : Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
      RH : LALRW.Rewriting_Handle := LALRW.Start_Rewriting (Unit.Context);

      function Process_Node
        (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status;

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node
        (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
      is
      begin
         case Node.Kind is
            when LALCO.Ada_Call_Stmt =>
               Handle_Log_Call (RH, Node.As_Call_Stmt);
            when others =>
               null;
         end case;
         return LALCO.Into;
      end Process_Node;

      use type LALCO.Ada_Node_Kind_Type;
   begin

      if Unit.Root.Kind /= LALCO.Ada_Compilation_Unit then
         raise Program_Error with "Unhandled multi compilation unit files";
      end if;

      Put_Line ("   [Ada]          " & Base_Name (Unit.Get_Filename));

      if Unit.Has_Diagnostics then
         Put_Line ("Invalid ada unit " & Unit.Get_Filename);
      else
         Unit.Root.Traverse (Process_Node'Access);

         declare
            Result : constant LALRW.Apply_Result := LALRW.Apply (RH);
         begin
            if Result.Success then
               Put_Line ("Success Rewrite");
               Output_Unit (Job_Ctx, Unit, Output_Dir.Get);
            else
               Put_Line ("Could not apply rewritings for " &
                           Result.Unit.Get_Filename);

               declare
                  use Langkit_Support.Diagnostics;

                  procedure Test (Cursor : Diagnostics_Vectors.Cursor);

                  procedure Test (Cursor : Diagnostics_Vectors.Cursor) is
                  begin
                     Put_Line (Langkit_Support.Diagnostics.To_Pretty_String (
                               Diagnostics_Vectors.Element (Cursor)));
                  end Test;
               begin
                  Result.Diagnostics.Iterate (Test'Access);
               end;

               raise Program_Error with "Could not apply rewritings";
            end if;
         end;
      end if;
   end Process_Unit;

   -----------
   -- Setup --
   -----------

   procedure Setup (Ctx   : Helpers.App_Context;
                    Jobs  : Helpers.App_Job_Context_Array)
   is
      pragma Unreferenced (Jobs, Ctx);
   begin
      Traces.Parse_Config_File;

      if Instrument_Mode.Get and then Decode_Mode.Get then
         Put_Line ("Only one mode can be used (" &
                     Instr_Mode_Long & " | " &
                     Decode_Mode_Long & ")");
         GNAT.OS_Lib.OS_Exit (1);

      elsif Instrument_Mode.Get then
         Put_Line ("Instrument");

      elsif Decode_Mode.Get then
         Offmt_Lib.Decoding.Decode (Map_Filename);
         GNAT.OS_Lib.OS_Exit (0);

      else
         Put_Line ("Missing mode switch (" &
                     Instr_Mode_Long & " | " &
                     Decode_Mode_Long & ")");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end Setup;

   -----------------
   -- Output_Unit --
   -----------------

   procedure Output_Unit
     (Job_Ctx      : Libadalang.Helpers.App_Job_Context;
      Unit         : Libadalang.Analysis.Analysis_Unit;
      Out_Dir_Path : Strings.XString)
   is
      pragma Unreferenced (Job_Ctx);
      use type Strings.XString;

      Content : constant String := LALU.Unparse (Unit.Root);

      Orig_Unit : constant VFS.Virtual_File :=
        VFS.Create (VFS."+" (Unit.Get_Filename));

      Unit_Base_Name : constant VFS.Filesystem_String :=
        VFS.Base_Name (Orig_Unit);
   begin
      if Out_Dir_Path = Strings.Null_XString then
         Put_Line ("--  " & VFS."+" (Unit_Base_Name));
         Put_Line (Content);
         New_Line;
      else
         declare
            Out_Dir : constant VFS.Virtual_File :=
              VFS.Create_From_Base (VFS."+" (Strings.To_String (Out_Dir_Path)));

            New_Unit : VFS.Writable_File :=
              VFS.Join (Out_Dir, Unit_Base_Name).Write_File;
         begin
            VFS.Write (New_Unit, Content);
            VFS.Close (New_Unit);
         end;
      end if;
   end Output_Unit;

   ----------------
   -- Output_Map --
   ----------------

   procedure Output_Map (Map : Offmt_Lib.Trace_Map) is
      use Offmt_Lib.Storage;

      Store_Res : constant Store_Result :=
        Offmt_Lib.Storage.Store (Map, Map_Filename);
   begin
      if Store_Res.Success then
         Put_Line ("Trace map saved.");
      else
         Put_Line ("Cannot store trace map: " & To_String (Store_Res.Msg));
      end if;
   end Output_Map;

   ------------------
   -- Post_Process --
   ------------------

   procedure Post_Process (Ctx : Helpers.App_Context;
                           Jobs : Helpers.App_Job_Context_Array)
   is
      pragma Unreferenced (Ctx, Jobs);
   begin
      Output_Map (All_Traces);
   end Post_Process;

begin
   App.Run;
end Offmt_Tool;
