with Ada.Containers.Indefinite_Vectors;

with Ada.Strings.Unbounded;
with Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;

with Geometry;

package ObjLoader_Utils is

   package S_U renames Ada.Strings.Unbounded;
   package S_M renames Ada.Strings.Maps;

   function UnboundedString_To_Float (su : S_U.Unbounded_String) return Float;

   function UnboundedString_To_Positive
     (su : S_U.Unbounded_String) return Positive;

   VERTEX_NORMAL         : constant Count_Type := 2;
   VERTEX_TEXTURE_NORMAL : constant Count_Type := 3;

   package Line_Components is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Natural,
        Element_Type => S_U.Unbounded_String,
        "="          => S_U."=");

   Whitespace : constant S_M.Character_Set := S_M.To_Set (' ');
   Slash      : constant S_M.Character_Set := S_M.To_Set ('/');

   function Tokenize_Line
     (Line : String; separator : S_M.Character_Set)
      return Line_Components.Vector;

   function Format_To_Vertex (Line : String) return Geometry.Vertex;
   function Format_To_Texture (Line : String) return Geometry.Texture;
   function Format_To_Normal (Line : String) return Geometry.Normal;
   function Format_To_Face (Line : String) return Geometry.Face;

end ObjLoader_Utils;
