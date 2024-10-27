with Ada.Containers.Indefinite_Vectors;

package Math.Geometry is

   package V_List is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Point,
        "="          => "=");

   package N_List is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Point,
        "="          => "=");

   type Texture is record

      u, v : Float;
   end record;

   package T_List is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Texture,
        "="          => "=");

   type Indices_List is array (Positive range 1 .. 3) of Positive;

   type Face is record

      Vertices_Indices : Indices_List;
      Textures_Indices : Indices_List;
      Normals_Indices  : Indices_List;

   end record;

   package F_List is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Face,
        "="          => "=");

   function Normal_From_Points (Anchor, A, B : Point) return Point;

   function Get_Face_Normals
     (Ns            : N_List.Vector;
      N_Indices     : Indices_List;
      Face_Vertices : V_List.Vector) return N_List.Vector;
   --
   --  In case they are in the .obj, they are just loaded.
   --  Otherwise, they are calculated on the fly in order to be more efficient.
   --
   --  To calculate a normal, one need three points A, B and C (we have a
   --  triangular face), and to choose an anchor point which will be the
   --  normal's point. For example, with C as anchor, Nc = CA ^ CB.
   --  (^ denote the vectorial product).
   --
   --  This means I have to get each point with some permutation of [1, 2, 3]
   --  if I want to automatically calculate Na, Nb, Nc from the array [A, B, C]
   --  (each point will be an anchor and a support point at some time).
   --
   --  In order to have the permutations of [1, 2, 3], we can calculate the
   --  functions Fn which Associate :
   --
   --  J -> 1 [1, 2, 3]
   --  J -> 2 [3, 1, 2]
   --  J -> 3 [2, 3, 1]
   --          |  |  |
   --         F1 F2 F3
   --
   --  This can easily be done because we only have 3 points and thus
   --  it can be solve by finding ax**2 + bx + c for each.
   --  Moreover, it can be noted that F3 is trivially 4 - J

end Math.Geometry;
