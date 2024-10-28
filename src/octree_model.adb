with Math.Octree;
with ObjLoader;

procedure Octree_Model is

   package M_O renames Math.Octree;
   package M_OS renames Math.Octree.Octree_Struct;

   O_Tree      : M_OS.Tree := M_OS.Empty_Tree;
   Root        : constant M_OS.Cursor := M_OS.Root (O_Tree);
   First_Child : M_OS.Cursor;

   Node : M_O.Octree_Node;

   Objs : ObjLoader.Scene;

begin

   ObjLoader.Loader ("../scenes/tree_part.obj", Objs);
   Node := (M_O.Get_Scene_Bounds (Objs.Vertex_List), Objs.Faces_List);
   M_OS.Append_Child (O_Tree, Root, Node);
   First_Child := M_OS.First_Child (Root);

   M_O.Create_Octree (O_Tree, Objs.Vertex_List, Objs.Faces_List);

   M_O.Print (First_Child);

end Octree_Model;
