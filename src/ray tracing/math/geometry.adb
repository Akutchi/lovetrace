with Ada.Numerics.Elementary_Functions;

with Ada.Text_IO;

package body Geometry is

   package N_EF renames Ada.Numerics.Elementary_Functions;
   package T_IO renames Ada.Text_IO;

   function "+" (u, v : Vertex) return Vertex is
   begin

      return (u.x + v.x, u.y + v.y, u.z + v.z, u.w);
   end "+";

   function "*" (λ : Float; u : Vertex) return Vertex is
   begin
      return (λ * u.x, λ * u.y, λ * u.z, u.w);

   end "*";

   function "*" (λ : Integer; u : Vertex) return Vertex is
      λ_f : constant Float := Float (λ);

   begin
      return (λ_f * u.x, λ_f * u.y, λ_f * u.z, u.w);

   end "*";

   function "-" (u, v : Vertex) return Vertex is
   begin
      return u + (Geometry."*" (-1, v));
   end "-";

   function "*" (u : Vertex; N : Normal) return Float is
   begin
      return u.x * N.x + u.y * N.y + u.z * N.z;
   end "*";

   procedure Print (v : Vertex) is
   begin

      T_IO.Put_Line
        (Float'Image (v.x)
         & " "
         & Float'Image (v.y)
         & " "
         & Float'Image (v.z));
   end Print;

   -----------
   -- Scale --
   -----------

   function Scale (s, u : Vertex) return Vertex is
   begin
      return (s.x * u.x, s.y * u.y, s.z * u.z, u.w);
   end Scale;

   ----------
   -- norm --
   ----------

   function norm (v : Vertex) return Vertex is

      vect_norm : Float;
      ε         : constant Float := 0.001;
   begin

      vect_norm :=
        N_EF.Sqrt ((v.x / v.w) ** 2 + (v.y / v.w) ** 2 + (v.z / v.w) ** 2);

      if vect_norm <= ε then
         return (0.0, 0.0, 0.0, 1.0);
      end if;

      return (v.x / vect_norm, v.y / vect_norm, v.z / vect_norm, 1.0);

   end norm;

   function norm (v : Normal) return Normal is

      vect_norm : Float;
      ε         : constant Float := 0.001;
   begin

      vect_norm := N_EF.Sqrt (v.x ** 2 + v.y ** 2 + v.z ** 2);

      if vect_norm <= ε then
         return (0.0, 0.0, 0.0);
      end if;

      return (v.x / vect_norm, v.y / vect_norm, v.z / vect_norm);

   end norm;

   ----------
   -- Turn --
   ----------

   function Turn (v : Vertex; axis : Character; α : Float) return Vertex is

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

   end Turn;

end Geometry;
