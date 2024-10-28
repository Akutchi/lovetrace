with Ada.Text_IO;

package body Math.Octree is

   package T_IO renames Ada.Text_IO;

   ----------------------
   -- Is_In_Dim_Bounds --
   ----------------------

   function Is_In_Dim_Bounds
     (V : Point; Bounds : Real_Vector; dim : Character) return Natural
   is

      min_x : constant Float := Float (Bounds (1));
      max_x : constant Float := Float (Bounds (2));
      min_y : constant Float := Float (Bounds (3));
      max_y : constant Float := Float (Bounds (4));
      min_z : constant Float := Float (Bounds (5));
      max_z : constant Float := Float (Bounds (6));

      ε : constant Float := 0.05;

   begin

      case dim is
         when 'x' =>
            return
              (if min_x + ε <= V.x and then V.x <= max_x - ε then 1 else 0);

         when 'y' =>
            return
              (if min_y + ε <= V.y and then V.y <= max_y - ε then 1 else 0);

         when 'z' =>
            return
              (if min_z + ε <= V.z and then V.z <= max_z - ε then 1 else 0);

         when others =>
            return 0;
      end case;

   end Is_In_Dim_Bounds;

   -----------------
   -- Is_In_Range --
   -----------------

   function Is_In_Range
     (Vs : V_List.Vector; Fi : F_List.Vector; Bounds : Real_Vector)
      return F_List.Vector
   is
      Fi_In_Bi : F_List.Vector;
   begin

      for F of Fi loop

         declare
            Idx       : constant Indices_List := F.Vertices_Indices;
            Vertex_In : constant Natural :=
              Is_In_Dim_Bounds (Vs (Idx (1)), Bounds, 'x')
              + Is_In_Dim_Bounds (Vs (Idx (1)), Bounds, 'y')
              + Is_In_Dim_Bounds (Vs (Idx (1)), Bounds, 'z');

         begin
            if Vertex_In >= 2 then
               F_List.Append (Fi_In_Bi, F);
            end if;
         end;
      end loop;

      return Fi_In_Bi;
   end Is_In_Range;

   ----------------
   -- Create_Box --
   ----------------

   function Create_Box
     (Ith : Positive; From : Octree_Node; Vs : V_List.Vector)
      return Octree_Node
   is
      Bi : Octree_Node;
   begin

      Bi.Bounds := M_List (Ith) * From.Bounds;
      Bi.Fi := Is_In_Range (Vs, From.Fi, Bi.Bounds);

      return Bi;

   end Create_Box;

   ----------------
   -- Next_Depth --
   ----------------

   procedure Next_Depth
     (O_Tree : in out Octree_Struct.Tree;
      Parent : in out Octree_Struct.Cursor;
      Vs     : V_List.Vector)
   is
      Parent_Box : constant Octree_Node := Octree_Struct.Element (Parent);
   begin

      if F_List.Length (Parent_Box.Fi) > MIN_DEPTH then

         for Ith in 1 .. 8 loop

            declare
               Bi : constant Octree_Node :=
                 Create_Box (Ith, From => Parent_Box, Vs => Vs);

               Current_Child : Octree_Struct.Cursor;
            begin

               Octree_Struct.Append_Child (O_Tree, Parent, Bi);
               Current_Child := Octree_Struct.Last_Child (Parent);
               Next_Depth (O_Tree, Current_Child, Vs);

            end;
         end loop;
      end if;

   end Next_Depth;

   ---------------------
   -- Liberate_Memory --
   ---------------------

   procedure Liberate_Memory
     (O_Tree : in out Octree_Struct.Tree; Box : in out Octree_Struct.Cursor)
   is

      Contents      : Octree_Node := Octree_Struct.Element (Box);
      Current_Child : Octree_Struct.Cursor;
   begin

      if not Octree_Struct.Is_Leaf (Box) then

         F_List.Clear (Contents.Fi);
         Octree_Struct.Replace_Element (O_Tree, Box, Contents);
         Current_Child := Octree_Struct.First_Child (Box);

         while Octree_Struct.Has_Element (Current_Child) loop
            Liberate_Memory (O_Tree, Current_Child);
            Current_Child := Octree_Struct.Next_Sibling (Current_Child);
         end loop;

      end if;

   end Liberate_Memory;

   -----------
   -- Print --
   -----------

   procedure Print (Box : Octree_Struct.Cursor) is

      Content : constant Octree_Node := Octree_Struct.Element (Box);
      Bounds  : constant Real_Vector := Content.Bounds;
      Tabs    :
        constant String (1 .. 2 * Positive (Octree_Struct.Depth (Box))) :=
          (others => ' ');

      F_Length : constant Count_Type := F_List.Length (Content.Fi);

   begin

      T_IO.Put
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

      T_IO.Put (" and has" & Count_Type'Image (F_Length) & " faces stored");

      T_IO.Put_Line (" ");

      if not Octree_Struct.Is_Leaf (Box) then
         Octree_Struct.Iterate_Children (Box, Print'Access);
      end if;

   end Print;

end Math.Octree;
