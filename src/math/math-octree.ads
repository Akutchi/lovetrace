with Ada.Containers.Multiway_Trees;

with Ada.Containers; use Ada.Containers;

with Math.Geometry; use Math.Geometry;

package Math.Octree is

   --  Thoses are the subdivision matrices that help us divide a square of
   --  bounds (mx, Mx, my, My, mz, Mz) into 8 smaller squares within it.

   M1 : constant Real_Matrix :=
     ((0.5, 0.5, 0.0, 0.0, 0.0, 0.0),
      (0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.5, 0.5, 0.0, 0.0),
      (0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 0.5, 0.5),
      (0.0, 0.0, 0.0, 0.0, 0.0, 1.0));

   M2 : constant Real_Matrix :=
     ((1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
      (0.5, 0.5, 0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.5, 0.5, 0.0, 0.0),
      (0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 0.5, 0.5),
      (0.0, 0.0, 0.0, 0.0, 0.0, 1.0));

   M3 : constant Real_Matrix :=
     ((1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
      (0.5, 0.5, 0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.5, 0.5, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 0.5, 0.5),
      (0.0, 0.0, 0.0, 0.0, 0.0, 1.0));

   M4 : constant Real_Matrix :=
     ((0.5, 0.5, 0.0, 0.0, 0.0, 0.0),
      (0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.5, 0.5, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 0.5, 0.5),
      (0.0, 0.0, 0.0, 0.0, 0.0, 1.0));

   M5 : constant Real_Matrix :=
     ((0.5, 0.5, 0.0, 0.0, 0.0, 0.0),
      (0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.5, 0.5, 0.0, 0.0),
      (0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 0.5, 0.5));

   M6 : constant Real_Matrix :=
     ((1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
      (0.5, 0.5, 0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.5, 0.5, 0.0, 0.0),
      (0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 0.5, 0.5));

   M7 : constant Real_Matrix :=
     ((1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
      (0.5, 0.5, 0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.5, 0.5, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 0.5, 0.5));

   M8 : constant Real_Matrix :=
     ((0.5, 0.5, 0.0, 0.0, 0.0, 0.0),
      (0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.5, 0.5, 0.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
      (0.0, 0.0, 0.0, 0.0, 0.5, 0.5));

   type Subdivision_List is
     array (Positive range 1 .. 8) of Real_Matrix (1 .. 6, 1 .. 6);

   M_List : constant Subdivision_List := (M1, M2, M3, M4, M5, M6, M7, M8);

   type Octree_Node is record

      Bounds : Real_Vector (1 .. 6) := (0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
      Fi     : F_List.Vector;
   end record;

   package Octree_Struct is new
     Ada.Containers.Multiway_Trees (Octree_Node, "=");

   procedure Next_Depth
     (O_Tree : in out Octree_Struct.Tree;
      Parent : in out Octree_Struct.Cursor;
      Vs     : V_List.Vector);

   procedure Liberate_Memory
     (O_Tree : in out Octree_Struct.Tree; Box : in out Octree_Struct.Cursor);
   --  When done creating the octree, if the node is not a leaf, we don't
   --  need the Fi's duplicates.

   procedure Print (Box : Octree_Struct.Cursor);

private

   MIN_DEPTH : constant Count_Type := 10;
   --  represent a region with <= 10 faces

   function Is_In_Dim_Bounds
     (V : Point; Bounds : Real_Vector; dim : Character) return Natural;

   function Is_In_Range
     (Vs : V_List.Vector; Fi : F_List.Vector; Bounds : Real_Vector)
      return F_List.Vector;

   function Create_Box
     (Ith : Positive; From : Octree_Node; Vs : V_List.Vector)
      return Octree_Node;

end Math.Octree;
