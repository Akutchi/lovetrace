with Ada.Strings;
with Ada.Strings.Fixed;

package body ObjLoader_Utils is

   package S renames Ada.Strings;
   package S_F renames Ada.Strings.Fixed;

   ------------------------------
   -- UnboundedString_To_Float --
   ------------------------------

   function UnboundedString_To_Float (su : S_U.Unbounded_String) return Float
   is
   begin
      return Float'Value (S_U.To_String (su));
   end UnboundedString_To_Float;

   ---------------------------------
   -- UnboundedString_To_Positive --
   ---------------------------------

   function UnboundedString_To_Positive
     (su : S_U.Unbounded_String) return Positive is
   begin
      return Positive'Value (S_U.To_String (su));
   end UnboundedString_To_Positive;

   -------------------
   -- Tokenize_Line --
   -------------------

   function Tokenize_Line
     (Line : String; separator : S_M.Character_Set)
      return Line_Components.Vector
   is

      F : Positive;
      L : Natural;
      I : Natural := 1;

      Token_List : Line_Components.Vector;

   begin

      while I in Line'Range loop

         S_F.Find_Token (Line, separator, I, S.Outside, F, L);
         exit when L = 0;

         Line_Components.Append
           (Token_List, S_U.To_Unbounded_String (Line (F .. L)));

         I := L + 1;

      end loop;

      return Token_List;

   end Tokenize_Line;

   ----------------------
   -- Format_To_Vertex --
   ----------------------

   function Format_To_Vertex (Line : String) return Geometry.Vertex is

      Token_List : Line_Components.Vector;
   begin

      Token_List := Tokenize_Line (Line, Whitespace);

      if Line_Components.Length (Token_List) = 4 then
         return
           (UnboundedString_To_Float (Token_List (1)),
            UnboundedString_To_Float (Token_List (2)),
            UnboundedString_To_Float (Token_List (3)),
            1.0);
      end if;

      return
        (UnboundedString_To_Float (Token_List (1)),
         UnboundedString_To_Float (Token_List (2)),
         UnboundedString_To_Float (Token_List (3)),
         UnboundedString_To_Float (Token_List (4)));

   end Format_To_Vertex;

   -----------------------
   -- Format_To_Texture --
   -----------------------

   function Format_To_Texture (Line : String) return Geometry.Texture is

      Token_List : Line_Components.Vector;
   begin

      Token_List := Tokenize_Line (Line, Whitespace);
      return
        (UnboundedString_To_Float (Token_List (1)),
         UnboundedString_To_Float (Token_List (2)),
         UnboundedString_To_Float (Token_List (3)));

   end Format_To_Texture;

   ----------------------
   -- Format_To_Normal --
   ----------------------

   function Format_To_Normal (Line : String) return Geometry.Normal is

      Token_List  : Line_Components.Vector;
      Normal_Vect : Geometry.Normal;
   begin

      Token_List := Tokenize_Line (Line, Whitespace);
      Normal_Vect :=
        (UnboundedString_To_Float (Token_List (1)),
         UnboundedString_To_Float (Token_List (2)),
         UnboundedString_To_Float (Token_List (3)));

      return Geometry.Norm (Normal_Vect);

   end Format_To_Normal;

   --------------------
   -- Format_To_Face --
   --------------------

   function Format_To_Face (Line : String) return Geometry.Face is

      Token_List, Tmp_List : Line_Components.Vector;

      Vertices, Textures, Normals : Geometry.Indices_List;

      I : Positive := 1;

   begin

      Token_List := Tokenize_Line (Line, Whitespace);

      for E of Token_List loop

         if S_U.To_String (E) /= "f" then

            Tmp_List := Tokenize_Line (S_U.To_String (E), Slash);

            Vertices (I) := UnboundedString_To_Positive (Tmp_List (0));

            case Line_Components.Length (Tmp_List) is

               when VERTEX_NORMAL =>
                  Textures (I) := 1;
                  Normals (I) := UnboundedString_To_Positive (Tmp_List (1));

               when VERTEX_TEXTURE_NORMAL =>
                  Textures (I) := UnboundedString_To_Positive (Tmp_List (1));
                  Normals (I) := UnboundedString_To_Positive (Tmp_List (2));

               when others =>
                  Textures (I) := 1;
                  Normals (I) := 1;

            end case;

            I := I + 1;

         end if;

      end loop;

      return (Vertices, Textures, Normals);

   end Format_To_Face;

end ObjLoader_Utils;
