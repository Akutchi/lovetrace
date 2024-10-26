with Ada.Numerics.Generic_Real_Arrays;

with ObjLoader;
with Camera;

with Geometry; use Geometry;
with Colors;   use Colors;

package Tracing is

   type Real is digits 4;

   package Lin_Alg is new Ada.Numerics.Generic_Real_Arrays (Real);
   use Lin_Alg;

   type Ray is tagged private;

   function Init_Ray
     (cam : Camera.Apparatus; dir : Vertex; t_min, t_max : Float) return Ray;

   function Cast (R : Ray; Objs : ObjLoader.Scene) return Color;

   type Hit is record

      Touched_Object  : Boolean := False;
      t               : Float := -1.0;
      NO_INTERSECTION : Float := -1.0;
      Normal_On_Touch : Normal;

   end record;

private

   type Ray is tagged record

      cam          : Camera.Apparatus;
      dir          : Vertex;
      t_min, t_max : Float;

   end record;

   function Is_In_Range (R : Ray; t : Float) return Boolean;

   function To_Camera_Coordinates (R : Ray; v : Vertex) return Vertex;

   function Point_In_Triangle (a, b, ε : Float) return Boolean;

   procedure Intersect (R : Ray; Vs : V_List.Vector; H : in out Hit);

end Tracing;
