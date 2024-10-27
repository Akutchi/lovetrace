with Ada.Text_IO;

with GNAT.Formatted_String; use GNAT.Formatted_String;

package body Math.Octree is

   package T_IO renames Ada.Text_IO;

   function Is_In_Range
     (Fi : F_List.Vector; Bounds : Real_Vector) return F_List.Vector
   is
      Fi_In_Bi : F_List.Vector;
   begin
      return Fi_In_Bi;
   end Is_In_Range;

   function Create_Box (Ith : Positive; From : Octree_Node) return Octree_Node
   is
      Bi : Octree_Node;
   begin

      Bi.Bounds := M_List (Ith) * From.Bounds;
      Bi.Fi := Is_In_Range (From.Fi, Bi.Bounds);

      return Bi;

   end Create_Box;

   procedure Next_Depth
     (O_Tree : in out Octree_Struct.Tree; Parent : in out Octree_Struct.Cursor)
   is
      Parent_Box : constant Octree_Node := Octree_Struct.Element (Parent);
   begin

      if F_List.Length (Parent_Box.Fi) > MIN_DEPTH then

         T_IO.Put_Line ("Entered");

         for Ith in 1 .. 8 loop

            declare
               Bi : constant Octree_Node :=
                 Create_Box (Ith, From => Parent_Box);

               Current_Child : Octree_Struct.Cursor;
            begin

               Octree_Struct.Append_Child (O_Tree, Parent, Bi);
               Current_Child := Octree_Struct.Last_Child (Parent);
               Next_Depth (O_Tree, Current_Child);

            end;
         end loop;
      end if;

   end Next_Depth;

   -----------
   -- Print --
   -----------

   procedure Print (Box : Octree_Struct.Cursor) is

      Content : Octree_Node := Octree_Struct.Element (Box);
      Bounds  : Real_Vector := Content.Bounds;
      Tabs    :
        constant String (1 .. 2 * Positive (Octree_Struct.Depth (Box))) :=
          (others => ' ');

   begin

      T_IO.Put_Line
        (Tabs
         & Real'Image (Bounds (1))
         & " "
         & Real'Image (Bounds (2))
         & " "
         & Real'Image (Bounds (3))
         & " "
         & Real'Image (Bounds (4))
         & " "
         & Real'Image (Bounds (5))
         & " "
         & Real'Image (Bounds (6)));

      T_IO.Put_Line (" ");

      if not Octree_Struct.Is_Leaf (Box) then
         Octree_Struct.Iterate_Children (Box, Print'Access);
      end if;

   end Print;

end Math.Octree;
