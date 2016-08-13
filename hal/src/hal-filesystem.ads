package HAL.Filesystem is

   subtype Pathname is String;

   type File_Kind is (Regular_File, Directory);

   type File_Mode is (Read_Only, Write_Only, Read_Write);

   type Permission_Kind  is (Others_Execute, Others_Write, Others_Read,
                             Group_Execute,  Group_Write,  Group_Read,
                             Owner_Execute,  Owner_Write,  Owner_Read);

   type Permission_Set is array (Permission_Kind) of Boolean;

   type Status_Kind is (Status_Ok,
                        Symbolic_Links_Loop,
                        Permission_Denied,
                        Input_Output_Error,
                        No_Such_File_Or_Directory,
                        Filename_Is_Too_Long,
                        Not_A_Directory,
                        Representation_Overflow,
                        Invalid_Argument,
                        Not_Enough_Space,
                        Not_Enough_Memory,
                        Bad_Address,
                        File_Exists,
                        Read_Only_File_System,
                        Operation_Not_Permitted,
                        No_Space_Left_On_Device,
                        Too_Many_Links,
                        Resource_Busy,
                        Buffer_Is_Too_Small,
                        Read_Would_Block,
                        Call_Was_Interrupted);

   type FS_Driver is limited interface;
   type FS_Driver_Ref is access all FS_Driver'Class;

   function Create_Node (This : in out FS_Driver;
                         Path : Pathname;
                         Kind : File_Kind)
                         return Status_Kind is abstract;
end HAL.Filesystem;
