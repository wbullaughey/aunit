------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    A U N I T . T E S T _ S U I T E S                     --
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
with AUnit.Memory.Utils;
with Ada_Lib.trace;use Ada_Lib.trace;

package body AUnit.Test_Suites is

   --------------
   -- Add_Test --
   --------------

   procedure Add_Test
     (S : access Test_Suite'Class;
      T : access Test_Suite'Class)
   is
   begin
      Log_In (Debug, "TS_Elt");
      Append
        (S.Tests,
         (Kind => TS_Elt,
          TS   => Access_Test_Suite'(T.all'Unchecked_Access)));
      Log_Out (Debug);
   end Add_Test;

   --------------
   -- Add_Test --
   --------------

   procedure Add_Test
     (S : access Test_Suite'Class;
      T : access AUnit.Simple_Test_Cases.Test_Case'Class)
   is
   begin
      Log_In (Debug, "TC_elt " & Quote ("suite", T.Name.all) &
         (if T.Routine_Name = Null then
            " no routine name"
         else
            Quote (" routine", T.Routine_Name.all)));
      Append
        (S.Tests,
         (Kind => TC_Elt,
          TC   => AUnit.Simple_Test_Cases.Test_Case_Access'(T.all'Unchecked_Access)));
      Log_Out (Debug);
   end Add_Test;

   ---------
   -- Run --
   ---------

   procedure Run (Suite   : access Test_Suite;
                  Options :        AUnit_Options;
                  R       : in out Result'Class;
                  Outcome :    out Status)
   is
      C      : Cursor := First (Suite.Tests);
      Result : Status := Success;

   begin
      Log_In (Debug, "AUnit_Options " & Image (Options'address));
      Outcome := Success;
      while Has_Element (C) loop
         Log_Here (Debug, "Element find " & Element (C).Kind'img);

         case Element (C).Kind is
            when TC_Elt =>
               AUnit.Simple_Test_Cases.Run (Element (C).TC, Options, R, Result);
            when TS_Elt =>
               Run (Element (C).TS, Options, R, Result);
         end case;

         if Result = Failure then
            Log_Here (Debug, "failed");
            Outcome := Failure;
         end if;

         Next (C);
      end loop;
      Log_Out (Debug);
   end Run;

   ---------------
   -- New_Suite --
   ---------------

   function New_Suite return Access_Test_Suite is
      type Access_Type is access all Test_Suite;
      pragma No_Strict_Aliasing (Access_Type);
      function Alloc is new AUnit.Memory.Utils.Gen_Alloc
        (Test_Suite, Access_Type);
      function Convert is new Ada.Unchecked_Conversion
        (Access_Type, Access_Test_Suite);
      Ret : constant Access_Type := Alloc;
      Obj : Test_Suite;
      for Obj'Address use Ret.all'Address;
      pragma Warnings (Off, Obj);
   begin
      return Convert (Ret);
   end New_Suite;

begin
--Debug := True;
   Log_Here (Debug);
end AUnit.Test_Suites;
