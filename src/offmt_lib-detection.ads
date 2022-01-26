with Libadalang.Analysis;

package Offmt_Lib.Detection is

   package LAL renames Libadalang.Analysis;

   function Check_Defmt_Log_Call (Node : LAL.Call_Stmt'Class;
                                  Call : out LAL.Call_Expr)
                                  return Boolean;
   --  Return True if Node is a valid Offmt.Log (<fmt>) call statement

end Offmt_Lib.Detection;
