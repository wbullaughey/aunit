------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          A U N I T . T E S T S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2009-2011, AdaCore                       --
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
-- GNAT is maintained by AdaCore (http://www.adacore.com).                  --
--                                                                          --
------------------------------------------------------------------------------

with AUnit.Simple_Test_Cases;  use AUnit.Simple_Test_Cases;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body AUnit.Test_Filters is

   function Starts_With (Str : String; Prefix : String) return Boolean;
   --  Whether Str starts with Prefix

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With (Str : String; Prefix : String) return Boolean is
   begin
      if Str'Length < Prefix'Length then
         return False;
      end if;

      return Str (Str'First .. Str'First + Prefix'Length - 1) = Prefix;
   end Starts_With;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Filter : in out Name_Filter; Name : String) is
   begin
      Log_Here (Debug, Quote (" Name", Name));
      Message_Free (Filter.Name);
      Filter.Name := Format (Name);
   end Set_Name;

   ---------------
   -- Is_Active --
   ---------------

   overriding
   function Is_Active
     (Filter : Name_Filter;
      T      : AUnit.Tests.Test'Class) return Boolean is
   begin
      Log_In (Debug, "Filter " & Image (Filter'address) &
         (if Filter.Name = Null then " no filter name "
         else " filter name '" &Filter.Name.all & "' ") &
         "tag " &tag_name (T'tag) & " test address " & Image_Pointer (T'address));

      if Filter.Name = null
        or else Filter.Name.all = ""
      then
         Log_Out (Debug, " no fiter its active");
         return True;
      end if;

      if T not in AUnit.Simple_Test_Cases.Test_Case'Class
        or else Name (AUnit.Simple_Test_Cases.Test_Case'Class (T)) = null
      then
         --  There is no name, so it doesn't match the filter
         Log_Out (Debug, " inactive");
         return False;
      end if;

      if Routine_Name (AUnit.Simple_Test_Cases.Test_Case'Class (T)) = null then
         declare
            Test_Name            : constant String := Name (AUnit.Simple_Test_Cases.Test_Case'Class (T)).all;
            Result               : constant Boolean := Starts_With (Test_Name, Filter.Name.all);

         begin
            Log_Out (Debug, " Test_NameTest_Name " & Quote (Test_Name) &
               " filter " & Quote (Filter.Name.all) & " result " & Result'img);
            return Result;
         end;
      else
         declare
            Test_Name            : constant String := Name (AUnit.Simple_Test_Cases.Test_Case'Class (T)).all;
            Test_Routine_Name    : constant String := Routine_Name (AUnit.Simple_Test_Cases.Test_Case'Class (T)).all;
            Test_Filter          : constant String := Test_Name & " : " & Test_Routine_Name;
            Result               : constant Boolean := Starts_With (Test_Filter, Filter.Name.all);

        begin
            Log_Out (Debug, " Test_Name " & Quote (Test_Name) &
               " test filter " & Quote (Test_Filter) &
               " test routine name " & Quote (Test_Routine_Name) &
               " filter " & Quote (Filter.Name.all) &
               " result " & Result'img);
            return Result;
         end;
      end if;
   end Is_Active;

end AUnit.Test_Filters;


