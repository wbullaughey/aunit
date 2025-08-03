------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ C A L L E R                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2008-2011, AdaCore                   --
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
with AUnit.Assertions;
with AUnit.Memory.Utils;
with Ada_Lib.Trace;


package body AUnit.Test_Caller is

   function New_Fixture is new AUnit.Memory.Utils.Gen_Alloc
     (Test_Fixture, Fixture_Access);

   The_Fixture_Object : constant Fixture_Access := New_Fixture;

   ------------
   -- Create --
   ------------

   procedure Create
     (TC   : out Test_Case'Class;
      Name : String;
      Test : Test_Method)
   is
   begin
      TC.Name    := Format (Name);
      TC.Method  := Test;
      TC.Fixture := The_Fixture_Object;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Name : String;
      Test : Test_Method) return Test_Case_Access
   is
      type Access_Type is access all Test_Case;
      function Alloc is new AUnit.Memory.Utils.Gen_Alloc
        (Test_Case, Access_Type);
      function Convert is new Ada.Unchecked_Conversion
        (Access_Type, Test_Case_Access);
      Ret : constant Test_Case_Access := Convert (Alloc);
   begin
      Create (Ret.all, Name, Test);
      return Ret;
   end Create;

   ----------
   -- Name --
   ----------

   overriding
   function Name (Test : Test_Case) return Message_String is
   begin
      return Test.Name;
   end Name;

   --------------
   -- Run_Test --
   --------------

   overriding
   procedure Run_Test (Test : in out Test_Case) is
   begin
      --  Before running the fixture's method, we need to make sure that
      --  the test Ids correspond so that a failure reported via Fixture is
      --  correctly understood as being part of Test.
      AUnit.Assertions.Copy_Id (Test, Test.Fixture.all);
      Test.Method (Test_Fixture (Test.Fixture.all));
   end Run_Test;

   ------------
   -- Set_Up --
   ------------

   overriding
   procedure Set_Up (Test : in out Test_Case) is
   begin
ada_lib.trace.log_here;
      Set_Up (Test.Fixture.all);
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   overriding
   procedure Tear_Down (Test : in out Test_Case) is

      use Ada_Lib.Trace;

   begin
      Log_In (Debug or Trace_Set_Up);
      Tear_Down (Test.Fixture.all);
      Log_Out (Debug or Trace_Set_Up);
   end Tear_Down;

begin
--Debug := True;
   Ada_Lib.Trace.Log_Here (Debug or Ada_Lib.Trace.Elaborate or
      Ada_Lib.Trace.Trace_Options);
end AUnit.Test_Caller;
