with Geometry;

package Camera is

   type Screen_Details is record

      Demi_Width, Demi_Height : Positive;
      Distance_From_The_Eye   : Positive;  --  z : axis opposed to the scene
      vision                  : Positive;
      x, y                    : Integer; --  (0, 0) is at the center

   end record;

   type Apparatus is record

      o                : Geometry.Vertex;
      screen           : Screen_Details;
      l, r, t, b, n, f : Positive;

   end record;

   function Create_Apparatus
     (o : Geometry.Vertex; Screen : Screen_Details) return Apparatus;

end Camera;
