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

      Ada.Text_IO.Put_Line
        (Float'Image (v.x) & " " & Float'Image (v.y) & " " &
         Float'Image (v.z));
   end Print;

   ----------
   -- norm --
   ----------

   function norm (v : Vertex) return Vertex is

      vect_norm : Float;
      ε         : constant Float := 0.001;
   begin

      vect_norm :=
        N_EF.Sqrt ((v.x / v.w)**2 + (v.y / v.w)**2 + (v.z / v.w)**2);

      if vect_norm <= ε then
         return (0.0, 0.0, 0.0, 1.0);
      end if;

      return (v.x / vect_norm, v.y / vect_norm, v.z / vect_norm, 1.0);

   end norm;

   function norm (v : Normal) return Normal is

      vect_norm : Float;
      ε         : constant Float := 0.001;
   begin

      vect_norm := N_EF.Sqrt (v.x**2 + v.y**2 + v.z**2);

      if vect_norm <= ε then
         return (0.0, 0.0, 0.0);
      end if;

      return (v.x / vect_norm, v.y / vect_norm, v.z / vect_norm);

   end norm;

end Geometry;
