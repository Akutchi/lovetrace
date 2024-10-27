with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Elementary_Functions;

package Math is

   type Real is digits 4;

   package Lin_Alg is new Ada.Numerics.Generic_Real_Arrays (Real);
   use Lin_Alg;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions (Float);
   use Functions;

   type Point is tagged record

      x, y, z : Float;
   end record;

   function "+" (p1, p2 : Point) return Point;

   function "*" (λ : Float; p : Point) return Point;
   function "*" (p1, p2 : Point) return Float;

   function "-" (p1, p2 : Point) return Point;

   function Normalize (p : Point) return Point;

   function Rotate (p : Point; axis : Character; α : Float) return Point;

   procedure Print (p : Point);

end Math;
