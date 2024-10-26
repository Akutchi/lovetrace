with Ada.Numerics.Elementary_Functions;

with Ada.Text_IO;

with GNAT.Formatted_String; use GNAT.Formatted_String;

package body Math.Geometry is

   package N_EF renames Ada.Numerics.Elementary_Functions;
   package T_IO renames Ada.Text_IO;

   -------
   -- + --
   -------

   function "+" (u, v : Vertex) return Vertex is
   begin

      return (u.x + v.x, u.y + v.y, u.z + v.z, u.w);
   end "+";

   function "+" (u, v : Normal) return Normal is
   begin

      return (u.x + v.x, u.y + v.y, u.z + v.z);
   end "+";

   -------
   -- * --
   -------

   function "*" (λ : Float; u : Vertex) return Vertex is
   begin
      return (λ * u.x, λ * u.y, λ * u.z, u.w);

   end "*";

   function "*" (λ : Float; u : Normal) return Normal is
   begin
      return (λ * u.x, λ * u.y, λ * u.z);

   end "*";

   function "*" (λ : Integer; u : Vertex) return Vertex is
      λ_f : constant Float := Float (λ);

   begin
      return (λ_f * u.x, λ_f * u.y, λ_f * u.z, u.w);

   end "*";

   function "*" (u : Vertex; N : Normal) return Float is
   begin
      return u.x * N.x + u.y * N.y + u.z * N.z;
   end "*";

   -------
   -- - --
   -------

   function "-" (u, v : Vertex) return Vertex is
   begin
      return u + (-1.0) * v;
   end "-";

   function "-" (u, v : Normal) return Normal is
   begin
      return u + (-1.0) * v;
   end "-";

   function "-" (n : Normal; v : Vertex) return Normal is
   begin
      return (n.x - v.x, n.y - v.y, n.z - v.z);
   end "-";

   ----------
   -- Norm --
   ----------

   function Norm (v : Vertex) return Vertex is

      vect_norm : Float;
      ε         : constant Float := 0.001;
   begin

      vect_norm :=
        N_EF.Sqrt ((v.x / v.w) ** 2 + (v.y / v.w) ** 2 + (v.z / v.w) ** 2);

      if vect_norm <= ε then
         return (0.0, 0.0, 0.0, 1.0);
      end if;

      return (v.x / vect_norm, v.y / vect_norm, v.z / vect_norm, 1.0);

   end Norm;

   function Norm (v : Normal) return Normal is

      vect_norm : Float;
      ε         : constant Float := 0.001;
   begin

      vect_norm := N_EF.Sqrt (v.x ** 2 + v.y ** 2 + v.z ** 2);

      if vect_norm <= ε then
         return (0.0, 0.0, 0.0);
      end if;

      return (v.x / vect_norm, v.y / vect_norm, v.z / vect_norm);

   end Norm;

   ------------
   -- Rotate --
   ------------

   function Rotate (v : Vertex; axis : Character; α : Float) return Vertex is

      R_α : constant Real := Real (α);

      M : constant Real_Matrix :=
        ((Cos (R_α), -Sin (R_α)), (Sin (R_α), Cos (R_α)));

   begin

      case axis is

         when 'y' =>

            declare
               R_v : constant Real_Vector := (Real (v.x), Real (v.z));
               R_u : constant Real_Vector := M * R_v;

            begin
               return
                 (Float (R_u (R_u'First)),
                  v.y,
                  Float (R_u (R_u'First + 1)),
                  1.0);
            end;

         when 'x' =>

            declare
               R_v : constant Real_Vector := (Real (v.y), Real (v.z));
               R_u : constant Real_Vector := M * R_v;

            begin
               return
                 (v.x,
                  Float (R_u (R_u'First)),
                  Float (R_u (R_u'First + 1)),
                  1.0);
            end;

         when others =>
            return v;

      end case;

   end Rotate;

   function Rotate (n : Normal; axis : Character; α : Float) return Normal is

      R_α : constant Real := Real (α);

      M : constant Real_Matrix :=
        ((Cos (R_α), -Sin (R_α)), (Sin (R_α), Cos (R_α)));

   begin

      case axis is

         when 'y' =>

            declare
               R_v : constant Real_Vector := (Real (n.x), Real (n.z));
               R_u : constant Real_Vector := M * R_v;

            begin
               return
                 (Float (R_u (R_u'First)), n.y, Float (R_u (R_u'First + 1)));
            end;

         when 'x' =>

            declare
               R_v : constant Real_Vector := (Real (n.y), Real (n.z));
               R_u : constant Real_Vector := M * R_v;

            begin
               return
                 (n.x, Float (R_u (R_u'First)), Float (R_u (R_u'First + 1)));
            end;

         when others =>
            return n;

      end case;

   end Rotate;

   ------------------------
   -- Normal_From_Points --
   ------------------------

   function Normal_From_Points (Anchor, A, B : Vertex) return Normal is

      AnA : constant Vertex := A - Anchor;
      AnB : constant Vertex := B - Anchor;
   begin

      return
        (AnA.y * AnB.z - AnA.z * AnB.y,
         -(AnA.x * AnB.z - AnA.z * AnB.x),
         (AnA.x * AnB.y - AnA.y * AnB.x));

   end Normal_From_Points;

   -----------
   -- Print --
   -----------

   procedure Print (v : Vertex) is

      Format : Formatted_String := +"(%2.f, %2.f, %2.f)";
   begin

      Format := Format & v.x & v.y & v.z;
      T_IO.Put_Line (-Format);
   end Print;

   procedure Print (v : Normal) is

      Format : Formatted_String := +"(%2.f, %2.f, %2.f)";
   begin

      Format := Format & v.x & v.y & v.z;
      T_IO.Put_Line (-Format);
   end Print;


end Math.Geometry;
