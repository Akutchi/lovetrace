with Ada.Text_IO;

package body Math.Octree is

   package T_IO renames Ada.Text_IO;

   function Get_Scene_Bounds (Vs : V_List.Vector) return Real_Vector is

      min_x, max_x, min_y, max_y, min_z, max_z : Float := 0.0;
   begin

      for V of Vs loop

         if V.x < min_x then
            min_x := V.x;

         elsif V.x >= max_x then
            max_x := V.x;
         end if;

         if V.y < min_y then
            min_y := V.y;

         elsif V.y >= max_y then
            max_y := V.y;
         end if;

         if V.z < min_z then
            min_z := V.z;

         elsif V.z >= max_z then
            max_z := V.z;
         end if;

      end loop;

      return
        (Real (min_x),
         Real (max_x),
         Real (min_y),
         Real (max_y),
         Real (min_z),
         Real (max_z));

   end Get_Scene_Bounds;

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

               Current_Child         : Octree_Struct.Cursor;
               Current_Child_Content : Octree_Node;
            begin

               Octree_Struct.Append_Child (O_Tree, Parent, Bi);
               Current_Child := Octree_Struct.Last_Child (Parent);
               Next_Depth (O_Tree, Current_Child, Vs);

               --  When done creating one branch, if the node is not a leaf,
               --  we don't need the Fi's duplicates that were temporarily
               --  stored.
               --  if not Octree_Struct.Is_Leaf (Current_Child) then

               --     Current_Child_Content :=
               --       Octree_Struct.Element (Current_Child);
               --     F_List.Clear (Current_Child_Content.Fi);
               --     Octree_Struct.Replace_Element
               --       (O_Tree, Current_Child, Current_Child_Content);
               --  end if;

            end;
         end loop;
      end if;

   end Next_Depth;

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
