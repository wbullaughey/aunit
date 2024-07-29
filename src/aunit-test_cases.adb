------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      A U N I T . T E S T _ C A S E S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2000-2011, AdaCore                   --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with AUnit.Options;              use AUnit.Options;
with AUnit.Test_Filters;         use AUnit.Test_Filters;
with Ada_Lib.Trace;use Ada_Lib.Trace;

package body AUnit.Test_Cases is

   package body Registration is separate;

   -----------------
   -- Add_Routine --
   -----------------

-- procedure Add_Routine (T : in out Test_Case'Class; Val : Routine_Spec) is
   procedure Add_Routine (T : in out Test_Case; Val : Routine_Spec) is
   begin
      Log_In (Debug, Quote ("routine name", Val.Routine_Name));
      Routine_Lists.Append (T.Routines, Val);
      Log_Out (Debug);
   end Add_Routine;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out Test_Case) is
   begin
      Log_In (Debug);
      Test.Routine.Routine (Test);
      Log_Out (Debug);
   end Run_Test;

   ---------
   -- Run --
   ---------

   procedure Run
     (Test    : access Test_Case;
      Options :        AUnit.Options.AUnit_Options;
      R       : in out Result'Class;
      Outcome :    out Status)
   is
      use Routine_Lists;
      Result : Status;
      C      : Cursor;
   begin
      Ada_Lib.Trace.Log_In (Debug, "run options " & Image (Options'address) &
         (if Options.Filter = null then " no filter" else " have filter " &
            Image (Options.Filter.all'address)) &
            " test tag " & Ada_Lib.Trace.Tag_Name (Test_Case'class (Test.all)'tag));
      Outcome := Success;
      Routine_Lists.Clear (Test.Routines);
Ada_Lib.Trace.Log_here;
      Register_Tests (Test_Case'Class (Test.all));
Ada_Lib.Trace.Log_here;
      Set_Up_Case (Test_Case'Class (Test.all));
Ada_Lib.Trace.Log_here;
      C := First (Test.Routines);

      while Has_Element (C) loop
         Test.Routine := Element (C);
Ada_Lib.Trace.Log_here;
         if Options.Filter = null
           or else Is_Active (Options.Filter.all, Test.all)
         then
            Ada_Lib.Trace.Log_Here (Debug);
            AUnit.Simple_Test_Cases.Run
              (AUnit.Simple_Test_Cases.Test_Case (Test.all)'Access,
               Options, R, Result);

            if Result = Failure then
               Outcome := Failure;
            end if;
         end if;

Ada_Lib.Trace.Log_here;
         Next (C);
      end loop;

Ada_Lib.Trace.Log_here;
      Tear_Down_Case (Test_Case'Class (Test.all));
      Ada_Lib.Trace.Log_Out (Debug);
   end Run;

   ------------------
   -- Routine_Name --
   ------------------

   function Routine_Name (Test : Test_Case) return Message_String is
   begin
      Log_Here (Debug, (if Test.Routine.Routine_Name = Null then
            "routine name not set"
         else
            Test.Routine.Routine_Name.all));
      return Test.Routine.Routine_Name;
   end Routine_Name;

   ------------------
   --  Set_Up_Case --
   ------------------

   procedure Set_Up_Case (Test : in out Test_Case) is
      --  Default
      pragma Unreferenced (Test);
   begin
      null;
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   procedure Tear_Down_Case (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Tear_Down_Case;

   package body Specific_Test_Case_Registration is

      ----------------------
      -- Register_Wrapper --
      ----------------------

      procedure Register_Wrapper
        (Test    : in out Specific_Test_Case'Class;
         Routine : Specific_Test_Routine;
         Name    : String)
      is
         function Conv is
            new Ada.Unchecked_Conversion (Specific_Test_Routine, Test_Routine);
      begin
         Registration.Register_Routine
           (Test_Case'Class (Test),
            Conv (Routine),
            Name);
      end Register_Wrapper;

   end Specific_Test_Case_Registration;

begin
--Debug := True;
--Trace_Options := True;
   Log_Here (Debug or Elaborate or Trace_Options);
end AUnit.Test_Cases;
