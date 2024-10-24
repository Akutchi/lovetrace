with Geometry;

package Camera is

   type Screen_Details is record

      top_right, bottom_left : Geometry.Vertex;
      vision                 : Integer;
      x, y                   : Integer;

   end record;

   type Apparatus is record

      o                : Geometry.Vertex;
      screen           : Screen_Details;
      l, r, t, b, n, f : Integer;

   end record;

   function Create_Apparatus
     (o : Geometry.Vertex; Screen : Screen_Details) return Apparatus;

end Camera;
