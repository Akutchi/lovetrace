with Ada.Numerics.Elementary_Functions;

package body Geometry is

   package N_EF renames Ada.Numerics.Elementary_Functions;

   ----------
   -- norm --
   ----------

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
