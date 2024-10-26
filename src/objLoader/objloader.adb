with Ada.Text_IO;

with ObjLoader_Utils; use ObjLoader_Utils;

package body ObjLoader is

   package T_IO renames Ada.Text_IO;

   -------------
   -- Is_Type --
   -------------

   function Is_Type (T, Line : String) return Boolean is
   begin
      return Line (Line'First .. Line'First + 1) = T;
   end Is_Type;

   ------------
   -- Loader --
   ------------

   procedure Loader (File_Name : String; Objs : in out Scene) is

      ObjFile : T_IO.File_Type;

   begin

      T_IO.Open (ObjFile, T_IO.In_File, File_Name);
      while not T_IO.End_Of_File (ObjFile) loop

         declare
            Line : constant String := T_IO.Get_Line (ObjFile);
         begin

            if Line /= "" then

               if Is_Type (VERTEX, Line) then
                  Math.Geometry.V_List.Append
                    (Objs.Vertex_List, Format_To_Vertex (Line));

               elsif Is_Type (TEXTURE, Line) then
                  Math.Geometry.T_List.Append
                    (Objs.Texture_List, Format_To_Texture (Line));

               elsif Is_Type (NORMAL, Line) then
                  Math.Geometry.N_List.Append
                    (Objs.Normal_List, Format_To_Normal (Line));

               elsif Is_Type (FACE, Line) then
                  Math.Geometry.F_List.Append
                    (Objs.Faces_List, Format_To_Face (Line));

               end if;
            end if;
         end;
      end loop;

   end Loader;

end ObjLoader;
