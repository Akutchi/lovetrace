with Ada.Text_IO;

with GNAT.Formatted_String; use GNAT.Formatted_String;

package body Math is

   package T_IO renames Ada.Text_IO;

   -------
   -- + --
   -------

   function "+" (p1, p2 : Point) return Point is
   begin

      return (p1.x + p2.x, p1.y + p2.y, p1.z + p2.z);
   end "+";

   -------
   -- * --
   -------

   function "*" (λ : Float; p : Point) return Point is
   begin
      return (λ * p.x, λ * p.y, λ * p.z);

   end "*";

   function "*" (p1, p2 : Point) return Float is
   begin
      return p1.x * p2.x + p1.y * p2.y + p1.z * p2.z;

   end "*";

   -------
   -- - --
   -------

   function "-" (p1, p2 : Point) return Point is
   begin
      return p1 + (-1.0) * p2;
   end "-";

   ------------------------
   -- Normal_From_Points --
   ------------------------

   function Normal_From_Points (Anchor, A, B : Point) return Point is

      AnA : constant Point := A - Anchor;
      AnB : constant Point := B - Anchor;
   begin

      return
        (AnA.y * AnB.z - AnA.z * AnB.y,
         -(AnA.x * AnB.z - AnA.z * AnB.x),
         (AnA.x * AnB.y - AnA.y * AnB.x));

   end Normal_From_Points;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (p : Point) return Point is

      vect_norm : Float;
      ε         : constant Float := 0.001;
   begin

      vect_norm := Sqrt (p.x ** 2 + p.y ** 2 + p.z ** 2);

      if vect_norm <= ε then
         return (0.0, 0.0, 0.0);
      end if;

      return (p.x / vect_norm, p.y / vect_norm, p.z / vect_norm);

   end Normalize;

   ------------
   -- Rotate --
   ------------

   function Rotate (p : Point; axis : Character; α : Float) return Point is

      M : constant Real_Matrix :=
        ((Real (Cos (α)), Real (-Sin (α))), (Real (Sin (α)), Real (Cos (α))));

   begin

      case axis is

         when 'y' =>

            declare
               R_p : constant Real_Vector := (Real (p.x), Real (p.z));
               R_u : constant Real_Vector := M * R_p;

            begin
               return
                 (Float (R_u (R_u'First)), p.y, Float (R_u (R_u'First + 1)));
            end;

         when 'x' =>

            declare
               R_p : constant Real_Vector := (Real (p.y), Real (p.z));
               R_u : constant Real_Vector := M * R_p;

            begin
               return
                 (p.x, Float (R_u (R_u'First)), Float (R_u (R_u'First + 1)));
            end;

         when others =>
            return p;

      end case;

   end Rotate;

   -----------
   -- Print --
   -----------

   procedure Print (p : Point) is

      Format : Formatted_String := +"(%2.f, %2.f, %2.f)";
   begin

      Format := Format & p.x & p.y & p.z;
      T_IO.Put_Line (-Format);
   end Print;

end Math;
