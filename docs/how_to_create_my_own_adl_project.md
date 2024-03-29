# How to create my own Ada Drivers Library project?

So you have played with the examples provided with Ada Drivers Library and now
you want to create your own project.

Here is how to do it.

## The project directory tree

The recommended directory tree for a project using Ada Drivers Library (ADL) is
as follow:

* project_root_dir/
   * src/ (Source directory of your project)
     * main.adb (Main procedure of your project)
   * my_project.gpr (Project file of your project)
   * Ada_Drivers_Library/ (Generated by the project wizard)
       * repo/ (The Ada_Drivers_Library repository)
       * src/ (Generated by the project wizard)
         * adl_config.ads (Generated by the project wizard)
       * ada_drivers_library.gpr (Generated by the project wizard)

## Cloning the repository

First let's create the root directory of our project:

```
$ mkdir project_root_dir
$ cd project_root_dir
```

Then we clone the Ada_Drivers_Library repository:

```$ git clone https://github.com/AdaCore/Ada_Drivers_Library.git Ada_Drivers_Library/repo/```

## Project Wizard

Next, we use the project wizard to configure and generate the ADL project.

The [project_wizard](script/project_wizard.py) provides an interface to
create an ADL project. This script will ask a series of questions to define the
ADL configuration of your project and then generate the different files
required to use ADL based on this configuration. The questions are for
instance, which board do you want to use or how many file-systems do you want
to be able to mount.

You can run the script like so:

```$ ./Ada_Drivers_Library/repo/scripts/project_wizard.py```

The project wizard generates two files:

 - A GNAT project file: This file defines a library project and include all the
   source directories required for the configuration that you provided, the
   run-time to be used (if/when possible), and the compilation flags.

   All the configuration keys are also defined in the project file, this allows
   you to use the values in your own project file based on ADL. For instance:

   ```
   with "Ada_Drivers_Library.gpr";

   project My_Project is
      case Ada_Drivers_Library.Board is
         when "This board" => [...]
         when "That board" => [...]
      end case;
   end My_Project;
   ```

 - An Ada configuration file: This file is an Ada package spec containing the
   definitions of configuration keys so that they can be used to inside the ADL
   code. For instance, the configuration key `Max_Mount_Points` is declared
   like so:
   
   ```
   package ADL_Config is
      Max_Mount_Points : constant := 2; -- From default value
   end ADL_Config;
   ```

   This constant is then used to create the array of mount points:

   ```
      subtype Mount_Index is Integer range 0 .. Max_Mount_Points;
      subtype Valid_Mount_Index is Mount_Index range 1 .. Max_Mount_Points;
      type Mount_Array is array (Valid_Mount_Index) of Mount_Record;
   ```

   You can also use the values of ADL_Config in your own packages if you want
   to.

## Project file and main procedure

The last step is to create the root project file and the main procedure.

`my_project.gpr`: This is the root project file that describes your project. It
takes some information from the project file created by the project wizard,
and add other part specific to your project, like the main procedure for
instance.

Here is an example of root project file:
```
with "Ada_Drivers_Library/ada_drivers_library.gpr";

project My_Project is
  
  for Languages use ("Ada");
  
  for Target use Ada_Drivers_Library'Target;
  for Runtime ("Ada") use Ada_Drivers_Library'Runtime ("Ada");
  
  for Source_Dirs use ("src");
  for Main use ("main.adb");
  
  for Object_Dir use "obj";
  for Exec_Dir use ".";
  for Create_Missing_Dirs use "True";
  
  package Compiler renames Ada_Drivers_Library.Compiler;

end My_Project;
```

`main.adb`: This is the main procedure of your project, the one that is
executed when the program starts. Here is a dummy version that you can use to
check that the project infrastructure is OK:

```
procedure Main is
begin
   null;
end Main;
```

## Compilation

You can now compile your project with gprbuild:

```$ gprbuild -j0 -P my_project.gpr```

or open the project in GNAT Programming Studio:

```$ gpr -P my_project.gpr```

## Comments and feedback are welcome

Whether you found this documentation, please let us know. Don't hesitate to
suggests fixes or improvement. Thanks in advance :)
