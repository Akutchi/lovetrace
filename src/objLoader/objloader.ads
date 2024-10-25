with Geometry;

package ObjLoader is

   VERTEX  : constant String := "v ";
   TEXTURE : constant String := "vt";
   NORMAL  : constant String := "vn";
   FACE    : constant String := "f ";

   type Scene is record

      Vertex_List  : Geometry.V_List.Vector;
      Texture_List : Geometry.T_List.Vector;
      Normal_List  : Geometry.N_List.Vector;
      Faces_List   : Geometry.F_List.Vector;
      Scale        : Geometry.Vertex;

   end record;

   procedure Loader (File_Name : String; Objs : in out Scene);

private

   function Is_Type (T, Line : String) return Boolean;

end ObjLoader;
