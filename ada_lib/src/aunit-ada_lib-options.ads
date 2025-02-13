--with Ada_Lib.Database;
with Ada_Lib.Options.Actual;
with Ada_Lib.Trace;

--pragma Warnings (On, "GNOGA");
-- needs to bin in ada_lib directory so gnoga can include it
package AUnit.Ada_Lib.Options is

   Failed                     : exception;

   type AUnit_Options_Type    is limited new Standard.Ada_Lib.Options.Actual.
                                 Nested_Options_Type with null record;

   type AUnit_Options_Access  is access AUnit_Options_Type;
   type AUnit_Options_Class_Access
                              is access all AUnit_Options_Type'class;
   type AUnit_Options_Constant_Class_Access
                              is access constant AUnit_Options_Type'class;

   overriding
   function Initialize (
     Options                     : in out AUnit_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

   type String_Access            is access constant String;

   overriding
   function Process_Option (
      Options                     : in out AUnit_Options_Type;
      Iterator                   : in out Standard.Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option                     : in     Standard.Ada_Lib.Options.Option_Type'class
   ) return Boolean;

private

   overriding
   procedure Program_Help (      -- common for all programs that use AUnit.Ada_Lib.Options
      Options                    : in     AUnit_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     Standard.Ada_Lib.Options.Help_Mode_Type);


   overriding
   procedure Trace_Parse (
      Options              : in out AUnit_Options_Type;
      Iterator             : in out Standard.Ada_Lib.Options.
                                       Command_Line_Iterator_Interface'class);

end AUnit.Ada_Lib.Options;
