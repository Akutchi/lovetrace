with Geometry;

package Camera is

   type Screen_Details is record

      Demi_Width, Demi_Height : Positive;
      Distance_From_The_Eye   : Positive;
      vision                  : Positive;
      x, y                    : Integer; --  (0, 0) is at the center

      MIN_X, MAX_X, MIN_Y, MAX_Y : Integer;

   end record;

   type Apparatus is record

      origin           : Geometry.Vertex;
      screen           : Screen_Details;
      l, r, t, b, n, f : Positive;

   end record;

   function Create_Apparatus
     (The_Eye                                          : Geometry.Vertex;
      Screen_Distance, Demi_Width, Demi_Height, vision : Integer)
      return Apparatus;

end Camera;
