with Math;

package Camera is

   type Screen_Details is record

      Demi_Width, Demi_Height : Positive;
      Distance_From_The_Eye   : Float;
      vision                  : Float;
      x, z                    : Integer; --  (0, 0) is at the center

      MIN_X, MAX_X, MIN_Z, MAX_Z : Integer;

   end record;

   type Apparatus is record

      origin           : Math.Point;
      screen           : Screen_Details;
      l, r, t, b       : Positive;
      n, f             : Float;
      alpha_z, alpha_x : Float;

   end record;

   function Create_Apparatus
     (The_Eye                                   : Math.Point;
      Screen_Distance, Vision, alpha_z, alpha_x : Float;
      Demi_Width, Demi_Height                   : Integer) return Apparatus;

   procedure Adjust_Origin (o : in out Math.Point; v : Math.Point);

end Camera;
