with Ada.Containers.Indefinite_Vectors;

package Geometry is

   type Vertex is record

      x, y, z, w : Float;
   end record;

   type Texture is record

      u, v, w : Float;
   end record;

   type Normal is record

      x, y, z : Float;
   end record;

   type FV_List is array (Positive range 1 .. 3) of Positive;

   type Face is record

      Face_Vertices : FV_List;
      Face_Textures : FV_List;
      Face_Normals  : FV_List;

   end record;

   package V_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Vertex, "=" => "=");

   package T_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Texture, "=" => "=");

   package N_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Normal, "=" => "=");

   package F_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Face, "=" => "=");

   function norm (v : Normal) return Normal;

end Geometry;
