with Ada.Containers.Indefinite_Vectors;

package Math.Geometry is

   type Indices_List is array (Positive range 1 .. 3) of Positive;

   type Vertex is record

      x, y, z, w : Float;
   end record;

   type Texture is record

      u, v, w : Float;
   end record;

   type Normal is record

      x, y, z : Float;
   end record;

   type Face is record

      Vertices_Indices : Indices_List;
      Textures_Indices : Indices_List;
      Normals_Indices  : Indices_List;

   end record;

   package V_List is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Vertex,
        "="          => "=");

   package T_List is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Texture,
        "="          => "=");

   package N_List is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Normal,
        "="          => "=");

   package F_List is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Face,
        "="          => "=");

   function "+" (u, v : Vertex) return Vertex;
   function "+" (u, v : Normal) return Normal;

   function "*" (λ : Float; u : Vertex) return Vertex;
   function "*" (λ : Float; u : Normal) return Normal;
   function "*" (λ : Integer; u : Vertex) return Vertex;
   function "*" (u : Vertex; N : Normal) return Float;

   function "-" (u, v : Vertex) return Vertex;
   function "-" (u, v : Normal) return Normal;
   function "-" (n : Normal; v : Vertex) return Normal;

   function Normalize (v : Vertex) return Vertex;
   function Normalize (v : Normal) return Normal;

   function Rotate (v : Vertex; axis : Character; α : Float) return Vertex;
   function Rotate (n : Normal; axis : Character; α : Float) return Normal;

   function Normal_From_Points (Anchor, A, B : Vertex) return Normal;

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

   procedure Print (v : Vertex);
   procedure Print (v : Normal);

end Math.Geometry;
