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

   function Norm (v : Vertex) return Vertex;
   function Norm (v : Normal) return Normal;

   function Rotate (v : Vertex; axis : Character; α : Float) return Vertex;
   function Rotate (n : Normal; axis : Character; α : Float) return Normal;

   function Normal_From_Points (Anchor, A, B : Vertex) return Normal;

   procedure Print (v : Vertex);
   procedure Print (v : Normal);

end Math.Geometry;
