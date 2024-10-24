with Ada.Numerics.Elementary_Functions;

package body Geometry is

   package N_EF renames Ada.Numerics.Elementary_Functions;

   function "+" (u, v : Vertex) return Vertex is
   begin

      return (u.x + v.x, u.y + v.y, u.z + v.z, u.w);
   end "+";

   function "*" (k : Float; u : Vertex) return Vertex is
   begin
      return (k * u.x, k * u.y, k * u.z, u.w);

   end "*";

   function "*" (k : Integer; u : Vertex) return Vertex is
      k_f : constant Float := Float (k);

   begin
      return (k_f * u.x, k_f * u.y, k_f * u.z, u.w);

   end "*";

   function "-" (u, v : Vertex) return Vertex is
   begin
      return u + (Geometry."*" (-1, v));
   end "-";

   function "*" (u : Vertex; N : Normal) return Float is
   begin
      return u.x * N.x + u.y * N.y + u.z * N.z;
   end "*";

   ----------
   -- norm --
   ----------

   function norm (v : Vertex) return Vertex is

      vect_norm : Float;
      epsilon   : constant Float := 0.001;
   begin

      vect_norm :=
        N_EF.Sqrt ((v.x / v.w)**2 + (v.y / v.w)**2 + (v.z / v.w)**2);

      if vect_norm <= epsilon then
         return (0.0, 0.0, 0.0, 1.0);
      end if;

      return (v.x / vect_norm, v.y / vect_norm, v.z / vect_norm, 1.0);

   end norm;

   function norm (v : Normal) return Normal is

      vect_norm : Float;
      epsilon   : constant Float := 0.001;
   begin

      vect_norm := N_EF.Sqrt (v.x**2 + v.y**2 + v.z**2);

      if vect_norm <= epsilon then
         return (0.0, 0.0, 0.0);
      end if;

      return (v.x / vect_norm, v.y / vect_norm, v.z / vect_norm);

   end norm;

end Geometry;
