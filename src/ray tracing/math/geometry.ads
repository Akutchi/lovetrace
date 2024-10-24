with Ada.Containers.Indefinite_Vectors;

package Geometry is

   type Vertex is record

      x, y, z, w : Float;
   end record;

   function "+" (u, v : Vertex) return Vertex;
   function "-" (u, v : Vertex) return Vertex;
   function "*" (λ : Float; u : Vertex) return Vertex;
   function "*" (λ : Integer; u : Vertex) return Vertex;
   function norm (v : Vertex) return Vertex;
   procedure Print (v : Vertex);

   type Texture is record

      u, v, w : Float;
   end record;

   type Normal is record

      x, y, z : Float;
   end record;

   function "*" (u : Vertex; N : Normal) return Float;
   function norm (v : Normal) return Normal;

   type Indices_List is array (Positive range 1 .. 3) of Positive;

   type Face is record

      Vertices_Indices : Indices_List;
      Textures_Indices : Indices_List;
      Normals_Indices  : Indices_List;

   end record;

   package V_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Vertex, "=" => "=");

   package T_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Texture, "=" => "=");

   package N_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Normal, "=" => "=");

   package F_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Face, "=" => "=");

end Geometry;
