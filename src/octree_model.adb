with Math.Octree;
with ObjLoader;

with Math; use Math;

procedure Octree_Model is

   package M_O renames Math.Octree;
   package M_OS renames Math.Octree.Octree_Struct;

   O_Tree      : M_OS.Tree := M_OS.Empty_Tree;
   Root        : constant M_OS.Cursor := M_OS.Root (O_Tree);
   First_Child : M_OS.Cursor;

   Node : M_O.Octree_Node;

   Objs : ObjLoader.Scene;

begin

   ObjLoader.Loader ("../scenes/sphere.obj", Objs);
   Node := ((-1.0, 1.0, -1.0, 1.0, -1.0, 1.0), Objs.Faces_List);
   M_OS.Append_Child (O_Tree, Root, Node);
   First_Child := M_OS.First_Child (Root);

   M_O.Next_Depth (O_Tree, First_Child);
   M_O.Print (First_Child);

end Octree_Model;
