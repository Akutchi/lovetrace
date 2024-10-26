with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Elementary_Functions;

package Math is

   type Real is digits 4;

   package Lin_Alg is new Ada.Numerics.Generic_Real_Arrays (Real);
   use Lin_Alg;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions (Real);
   use Functions;

end Math;
