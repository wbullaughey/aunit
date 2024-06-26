------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2015-2020, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Vectors;

with GNAT.HTable;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with GPR; use GPR;
with GPR.ALI; use GPR.ALI;
with GPR.Env;
with GPR.Opt;   use GPR.Opt;
with GPR.Osint; use GPR.Osint;
with GPR.Tree;

package Gprls is

private

   type ALI_Kind is record
      File : File_Name_Type;
      Spec : Boolean;
   end record;

   function Hash (A : ALI_Kind) return GPR.Header_Num;

   package ALI_Names is new GNAT.HTable.Simple_HTable
     (Header_Num => GPR.Header_Num,
      Element    => GPR.Source_Id,
      No_Element => GPR.No_Source,
      Key        => ALI_Kind,
      Hash       => Hash,
      Equal      => "=");

   Initialized : Boolean := False;
   --  Set to True by the first call to Initialize.
   --  To avoid reinitialization of some packages.

   Save_Verbose : Boolean := False;
   Save_Verbosity_Level : Verbosity_Level_Type;

   Very_Verbose_Mode : Boolean := False;
   --  Set to True with switch -V

   Project_Search_Path : constant String := "Project Search Path:";
   --  Label displayed in verbose mode before the directories in the project
   --  search path. Do not modify without checking NOTE above.

   Prj_Path : GPR.Env.Project_Search_Path;

   Max_Column : constant := 80;

   No_Runtime : Boolean := False;
   --  Set to True if there is no default runtime and --RTS= is not specified

   type File_Status is (
     OK,                  --  matching timestamp
     Checksum_OK,         --  only matching checksum
     Not_Found,           --  file not found on source PATH
     Not_Same);           --  neither checksum nor timestamp matching

   type Dir_Data;
   type Dir_Ref is access Dir_Data;

   type Dir_Data is record
      Value : String_Access;
      Next  : Dir_Ref;
   end record;
   --  Simply linked list of dirs

   First_Source_Dir : Dir_Ref;
   Last_Source_Dir  : Dir_Ref;
   --  The list of source directories from the command line.
   --  These directories are added using Osint.Add_Src_Search_Dir
   --  after those of the GNAT Project File, if any.

   First_Lib_Dir : Dir_Ref;
   Last_Lib_Dir  : Dir_Ref;
   --  The list of object directories from the command line.
   --  These directories are added using Osint.Add_Lib_Search_Dir
   --  after those of the GNAT Project File, if any.

   Main_File : File_Name_Type;
   Ali_File  : File_Name_Type;
   Text      : Text_Buffer_Ptr;
   Next_Arg  : Positive;

   Selective_Output : Boolean := False;
   Print_Usage      : Boolean := False;
   Print_Unit       : Boolean := True;
   Print_Source     : Boolean := True;
   Print_Object     : Boolean := True;
   --  Flags controlling the form of the output

   Dependable        : Boolean := False;  --  -d
   --  Command line flags

   Closure           : Boolean := False;  --  --closure
   --  Get the closures of mains

   RTS_Specified : String_Access := null;
   --  Used to detect multiple use of --RTS= switch

   Exit_Status : Exit_Code_Type := E_Success;
   --  Reset to E_Fatal if bad error found

   type File_Name_Source (Name_Len : Natural) is record
      Source    : GPR.Source_Id;
      Tree      : Project_Tree_Ref;
      The_ALI   : ALI_Id;
      File_Name : String (1 .. Name_Len);
   end record;

   package File_Name_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, File_Name_Source);
   File_Names : File_Name_Vectors.Vector;
   --  As arguments are scanned, file names are stored in this array. The array
   --  is extensible, because there may be more files than arguments on the
   --  command line.

   No_Files_In_Command_Line : Boolean;
   --  Set this flag just after command line parsing from File_Names.Is_Empty

   Tree : constant GPR.Project_Node_Tree_Ref := new Project_Node_Tree_Data;
   --  The project tree where the project file is parsed

   Root_Environment : GPR.Tree.Environment;

   --  Packages of project files where unknown attributes are errors

   Naming_String   : aliased String := "naming";
   Builder_String  : aliased String := "builder";
   Compiler_String : aliased String := "compiler";
   Binder_String   : aliased String := "binder";
   Linker_String   : aliased String := "linker";
   --  Name of packages to be checked when parsing/processing project files

   List_Of_Packages : aliased String_List :=
                        (Naming_String'Access,
                         Builder_String'Access,
                         Compiler_String'Access,
                         Binder_String'Access,
                         Linker_String'Access);
   Packages_To_Check : constant String_List_Access := List_Of_Packages'Access;
   --  List of the packages to be checked when parsing/processing project files

   procedure Add_ALI
     (ALI_Name : File_Name_Type;
      Spec     : Boolean;
      Source   : GPR.Source_Id);
   --  Add ALI_Name to hash table ALI_Names

   procedure Add_File
     (File_Name : String;
      Tree      : Project_Tree_Ref;
      Source    : GPR.Source_Id := No_Source);
   --  Add File_Name to File_Names

   function Find_ALI (Source : GPR.Source_Id) return ALI_Id;
   --  Get the ALI_Id for the source

   function Find_Source
     (ALI_Name : File_Name_Type;
      Spec     : Boolean)
      return GPR.Source_Id;
   --  Find the source corresponding to an ALI file name

   procedure Find_Status
     (Source : GPR.Source_Id;
      ALI    : ALI_Id;
      Status : out File_Status);
   procedure Find_Status
     (Source : GPR.Source_Id;
      ALI    : ALI_Id;
      U      : Unit_Id;
      Status : out File_Status);
   --  Determine the file status (Status) of a source file

   function Corresponding_Sdep_Entry (A : ALI_Id; U : Unit_Id) return Sdep_Id;
   --  Give the Sdep entry corresponding to the unit U in ali record A

   procedure Output_Object (O : File_Name_Type);
   --  Print out the name of the object when requested

   procedure Output_Source (Tree : Project_Tree_Ref; Sdep_I : Sdep_Id);
   --  Print out the name and status of the source corresponding to this
   --  sdep entry.

   procedure Output_Source
     (Source : GPR.Source_Id; Sdep_I : Sdep_Id);

   procedure Output_Status (FS : File_Status; Verbose : Boolean);
   --  Print out FS either in a coded form if verbose is false or in an
   --  expanded form otherwise.

   procedure Output_Unit (U_Id : Unit_Id);
   --  Print out information on the unit when requested

   procedure Reset_Print;
   --  Reset Print flags properly when selective output is chosen

   --  procedure Search_RTS (Name : String);
   --  Find include and objects path for the RTS name.

   Project_Tree : constant Project_Tree_Ref :=
                    new Project_Tree_Data (Is_Root_Tree => True);
   --  The project tree

   type Path_Record;
   type Path_Access is access Path_Record;
   type Path_record is record
      Path : String_Access := null;
      Next : Path_Access   := null;
   end record;

   type Paths is record
      First : Path_Access := null;
      Last  : Path_Access := null;
   end record;
   No_Paths : constant Paths := (null, null);

   procedure Add (Path : String; To : in out Paths);

   procedure Get_Runtime_Source_Dirs
     (Project    : Project_Id;
      Tree       : Project_Tree_Ref;
      With_State : in out Paths);

   procedure Get_All_Runtime_Source_Dirs is
      new For_Every_Project_Imported (Paths, Get_Runtime_Source_Dirs);

   package GNATDIST is

      --  Any modification to this subunit requires synchronization with the
      --  GNATDIST sources.

      procedure Output_ALI (FNS : File_Name_Source);
      --  Output the unit information for GNATDIST

      procedure Output_No_ALI (FNS : File_Name_Source);
      --  Indicate that an ALI file cannot be found

   end GNATDIST;

end Gprls;
