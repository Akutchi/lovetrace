with Math.Geometry;

package ObjLoader is

   type Scene is record

      Vertex_List  : Math.Geometry.V_List.Vector;
      Texture_List : Math.Geometry.T_List.Vector;
      Normal_List  : Math.Geometry.N_List.Vector;
      Faces_List   : Math.Geometry.F_List.Vector;

   end record;

   procedure Loader (File_Name : String; Objs : in out Scene);
private

   VERTEX  : constant String := "v ";
   TEXTURE : constant String := "vt";
   NORMAL  : constant String := "vn";
   FACE    : constant String := "f ";

   function Is_Type (T, Line : String) return Boolean;

end ObjLoader;
