with ObjLoader;

with Geometry; use Geometry;
with Colors;   use Colors;

package Tracing is

   type Ray is tagged private;

   function Init_Ray (o, dir : Vertex; t_min, t_max : Float) return Ray;

   function Value_At (R : Ray; t : Float) return Vertex;

   function t_min (R : Ray) return Float;

   function t_max (R : Ray) return Float;

   function origin (R : Ray) return Vertex;

   function dir (R : Ray) return Vertex;

   function Cast (R : Ray; Objs : ObjLoader.Scene) return Color;

   type Hit is record

      Touched_Object  : Boolean := False;
      t               : Float;
      Normal_On_Touch : Normal;

   end record;

private

   type Ray is tagged record

      o            : Vertex;
      dir          : Vertex;
      t_min, t_max : Float;

   end record;

   procedure Intersect (Vs : V_List.Vector; R : Ray; H : in out Hit);

end Tracing;
