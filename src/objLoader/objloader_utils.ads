with Ada.Containers.Indefinite_Vectors;

with Ada.Strings.Unbounded;
with Ada.Strings.Maps;

with Ada.Containers; use Ada.Containers;

with Math.Geometry;

package ObjLoader_Utils is

   package S_U renames Ada.Strings.Unbounded;
   package S_M renames Ada.Strings.Maps;

   function Format_To_Vertex (Line : String) return Math.Point;
   function Format_To_Texture (Line : String) return Math.Geometry.Texture;
   function Format_To_Normal (Line : String) return Math.Point;
   function Format_To_Face (Line : String) return Math.Geometry.Face;

private

   Whitespace : constant S_M.Character_Set := S_M.To_Set (' ');
   Slash      : constant S_M.Character_Set := S_M.To_Set ('/');

   Has_No_W              : constant Count_Type := 4;
   VERTEX                : constant Count_Type := 0; --  f 1 2 3
   VERTEX_NORMAL         : constant Count_Type := 2; --  f 1//1
   VERTEX_TEXTURE_NORMAL : constant Count_Type := 3; --  f 1/1/1

   package Line_Components is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Natural,
        Element_Type => S_U.Unbounded_String,
        "="          => S_U."=");

   function UnboundedString_To_Float (su : S_U.Unbounded_String) return Float;

   function UnboundedString_To_Positive
     (su : S_U.Unbounded_String) return Positive;

   function Tokenize_Line
     (Line : String; separator : S_M.Character_Set)
      return Line_Components.Vector;

   function Has_W (Vertex_Components : Line_Components.Vector) return Boolean;

end ObjLoader_Utils;
