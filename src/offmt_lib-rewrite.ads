with Libadalang.Analysis;
with Libadalang.Rewriting;
with Libadalang.Common;

package Offmt_Lib.Rewrite is

   package LAL     renames Libadalang.Analysis;
   package LALCO   renames Libadalang.Common;
   package LALRW   renames Libadalang.Rewriting;

   procedure Rewrite_Log_Call (RH   : LALRW.Rewriting_Handle;
                               Node : LAL.Call_Stmt'Class;
                               T    : Trace);

end Offmt_Lib.Rewrite;
