with ObjLoader;
with Camera;
with Sources;

with Math.Geometry; use Math.Geometry;
with Colors;        use Colors;

package Math.Tracing is

   type Ray is tagged private;

   function Init_Ray
     (cam : Camera.Apparatus; dir : Vertex; t_min, t_max : Float) return Ray;

   function Value_At
     (R : Ray; t : Float; Ray_From_Camera : Boolean) return Vertex;

   function Light_Intensity
     (R     : Ray;
      at_P  : Vertex;
      N     : Normal;
      Light : Sources.Abstract_Source'Class) return Color;

   function Cast
     (R : Ray; Objs : ObjLoader.Scene; Light : Sources.Abstract_Source'Class)
      return Color;

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
   function To_Camera_Coordinates (R : Ray; n : Normal) return Normal;

   function Point_In_Triangle (a, b, ε : Float) return Boolean;

   procedure Intersect
     (R : Ray; Vs : V_List.Vector; Ns : N_List.Vector; H : in out Hit);

end Math.Tracing;
