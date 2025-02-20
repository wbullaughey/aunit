with Ada.Text_IO; use Ada.Text_IO;
--with Ada_Lib.Options;
with Ada_Lib.Help;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Options.Runstring;
--with Debug_Options;

--pragma Elaborate_All (Standard.Ada_Lib.Command_Line_Iterator);
package body AUnit.Ada_Lib.Options is

   Debug                         : aliased Boolean := False;
   Debug_Options                 : aliased Boolean := False;
   Trace_Option                  : constant Character := 'A';
   Options_With_Parameters       : aliased constant
                                    Standard.Ada_Lib.Options.
                                       Options_Type :=
                                          Standard.Ada_Lib.Options.
                                             Create_Options (Trace_Option,
                                                Standard.Ada_Lib.Options.Unmodified);
   Options_Without_Parameters    : aliased constant
                                    Standard.Ada_Lib.Options.
                                       Options_Type := Standard.Ada_Lib.
                                          Options.Null_Options;

   ----------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Aunit_Program_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug_Options or Trace_Options, "from " & From);
      Standard.Ada_Lib.Options.Runstring.Options.Register (
         Standard.Ada_Lib.Options.Runstring.With_Parameters,
         Options_With_Parameters);
      Standard.Ada_Lib.Options.Runstring.Options.Register (
         Standard.Ada_Lib.Options.Runstring.Without_Parameters,
         Options_Without_Parameters);
      Log_Out (Debug_Options or Trace_Options);
      return True;
   end Initialize;

   ----------------------------------------------------------------------------
   overriding
   function Process_Option (
      Options                    : in out Aunit_Program_Options_Type;
      Iterator                   : in out Standard.Ada_Lib.Options.
                                             Command_Line_Iterator_Interface'class;
      Option                     : in     Standard.Ada_Lib.Options.Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

      use Standard.Ada_Lib.Options;

   begin
      Log_In (Debug_Options or Trace_Options, Option.Image);
      if Has_Option (Option,
            Options_With_Parameters, Options_Without_Parameters) then
         if Option.Kind = Plain then
            case Option.Option is

               when Trace_Option =>   -- options for AUnit
                  Options.Trace_Parse (Iterator);

               when Others =>
                  return Log_Out (False, Debug_Options or Trace_Options);

            end case;
         else
            return Log_Out (False, Debug_Options or Trace_Options);
         end if;
      else
         return Log_Out (False, Debug_Options or Trace_Options);
            -- derived from Interface_Options_Type
      end if;

      return Log_Out (True, Debug_Options or Trace_Options,
         " " & Option.Image & " handled");
   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in      Aunit_Program_Options_Type;
      Help_Mode                  : in      Standard.Ada_Lib.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug_Options or Trace_Options);
      case Help_Mode is

      when Standard.Ada_Lib.Options.Program =>
            Standard.Ada_Lib.Help.Add_Option (Trace_Option, "trace options",
               "AUnit library traces", "AUnit library");

      when Standard.Ada_Lib.Options.Traces =>
         Put_Line ("AUnit.Ada_Lib.Options library trace options (-" & Trace_Option & ")");
         Put_Line ("      a               all");
         Put_Line ("      A               AUnit.Debug");
         Put_Line ("      l               AUnit.Ada_Lib.Debug");
         Put_Line ("      o               AUnit options");
         New_Line;

      end case;

      Log_Out (Debug_Options or Trace_Options);
   end Program_Help;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options        : in out Aunit_Program_Options_Type;
      Iterator       : in out Standard.Ada_Lib.Options.
                                 Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

      Parameter                  : constant String := Iterator.Get_Parameter;

   begin
      for Index in Parameter'range  loop
         declare
            Trace    : constant Character := Parameter (Index);

         begin
            case Trace is

               when 'a' =>
                  Debug := True;
                  Debug_Options := True;

               when 'A' =>
                  AUnit.Debug := True;

               when 'l' =>
                  Debug := True;

               when 'o' =>
                  Debug_Options := True;

               when others =>
                  Options.Bad_Option (Trace);

            end case;


         end;
      end loop;
   end Trace_Parse;

begin
AUnit.debug := True;
--debug := True;
--Trace_Options := True;
   Log_Here (Elaborate);
end AUnit.Ada_Lib.Options;
