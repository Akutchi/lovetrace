with ObjLoader;
with Camera;
with Sources;

with Math.Geometry; use Math.Geometry;
with Colors;        use Colors;

package Math.Tracing is

   type Ray is tagged private;

   function Init_Ray
     (cam : Camera.Apparatus; dir : Point; t_min, t_max : Float) return Ray;

   function Value_At
     (R : Ray; t : Float; Ray_From_Camera : Boolean) return Point;

   function Light_Intensity
     (R : Ray; at_P, N : Point; Light : Sources.Abstract_Source'Class)
      return Color;

   function Cast
     (R : Ray; Objs : ObjLoader.Scene; Light : Sources.Abstract_Source'Class)
      return Color;

   type Hit is record

      Touched_Object  : Boolean := False;
      t               : Float := -1.0;
      NO_INTERSECTION : Float := -1.0;
      Normal_On_Touch : Point;

   end record;

private

   type Ray is tagged record

      cam          : Camera.Apparatus;
      dir          : Point;
      t_min, t_max : Float;

   end record;

   function Is_In_Range (R : Ray; t : Float) return Boolean;

   function To_Camera_Coordinates (R : Ray; v : Point) return Point;

   function Point_In_Triangle (a, b, ε : Float) return Boolean;

   procedure Intersect
     (R : Ray; Vs : V_List.Vector; Ns : N_List.Vector; H : in out Hit);

end Math.Tracing;
