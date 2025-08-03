------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                A U N I T . S I M P L E _ T E S T _ C A S E S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                        Copyright (C) 2008-2012, AdaCore                  --
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

with Ada.Exceptions;
with Ada.Text_IO;
with AUnit.Assertions;   use AUnit.Assertions;
with AUnit.Options;      use AUnit.Options;
with AUnit.Test_Filters; use AUnit.Test_Filters;
with Ada_Lib.trace;use Ada_Lib.trace;
package body AUnit.Simple_Test_Cases is

   procedure Run_Routine
     (Test    : access Test_Case'Class;
      Options :        AUnit_Options;
      R       : in out Result'Class;
      Outcome :    out Status);
   --  Run one test routine

   -----------------
   -- Run_Routine --
   -----------------

   procedure Run_Routine
     (Test    : access Test_Case'Class;
      Options :        AUnit_Options;
      R       : in out Result'Class;
      Outcome :    out Status) is separate;

   ------------------
   -- Routine_Name --
   ------------------

   function Routine_Name (Test : Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      Log_In (Debug, "not overridden");
      return null;
   end Routine_Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      Log_Here (Debug or Trace_Set_Up);
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      Log_Here (Debug or Trace_Set_Up);
   end Tear_Down;

   ---------
   -- Run --
   ---------

   procedure Run
     (Test    : access Test_Case;
      Options :        AUnit_Options;
      R       : in out Result'Class;
      Outcome :    out Status)
   is
      Old : constant Test_Access := AUnit.Assertions.Current_Test;
      Setup_Completed : Boolean := True;
   begin
      Log_In (Debug, Quote (" suite", Test_Case'class (Test.all).Name) &
         Quote (" routine", Test_Case'class (Test.all).Routine_Name));
--     " tag " &tag_name (Test_Case'class(Test.all)'tag) &
--       " test address " & Image_Pointer (Test.all'address));
      Log_Here (Debug, Quote ("check running suite", Test_Case'class (Test.all).Name) &
         Quote (" routine", Test_Case'class (Test.all).Routine_Name));
      Outcome := Success;
      if Options.Filter = null
        or else Is_Active (Options.Filter.all, Test.all)
      then
         Ada.Text_IO.Put_Line (Quote ("running suite", Test_Case'class (Test.all).Name) &
            Quote (" routine", Test_Case'class (Test.all).Routine_Name));
         Log_Here (Debug);
         AUnit.Assertions.Set_Current_Test (Test_Access (Test));
         Init_Test (Test.all);
         Start_Test (R, 1);

         --  Run test routine
         begin
            Log_Here (Debug);
            Set_Up (Test_Case'Class (Test.all));
         exception
            when Fault: others =>
               Setup_Completed := False;
               Trace_Exception (Fault);
               AUnit.Assertions.Record_Assertion (R, Test_Case'class (Test.all).Name,
                  Test_Case'class (Test.all).Routine_Name,
                  (Format ("exception:" & Ada.Exceptions.Exception_Name (Fault)),
                   Format ("message:" & Ada.Exceptions.Exception_Message (Fault)), 0));
--             raise;
         end;

         if Setup_Completed then
            Log_Here (Debug);
            Run_Routine (Test, Options, R, Outcome);
            Log_Here (Debug);
         end if;
         Log_Here (Debug);
         Tear_Down (Test_Case'Class (Test.all));
         AUnit.Assertions.Set_Current_Test (Old);
      else
         Log_Here (Debug);
      end if;
      Log_Out (Debug, " outcome " & Outcome'img);
   end Run;

begin
--Debug := True;
   Log_Here (Elaborate or Trace_Options);
end AUnit.Simple_Test_Cases;
