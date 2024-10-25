with Ada.Text_IO;

package body Camera is

   function Create_Apparatus
     (The_Eye                                          : Geometry.Vertex;
      Screen_Distance, Demi_Width, Demi_Height, vision : Integer)
      return Apparatus
   is

      n : constant Positive := Integer (The_Eye.z) - Screen_Distance;
      f : constant Positive := abs (Integer (The_Eye.z) - vision);

      screen : Screen_Details;
      cam    : Apparatus;

   begin

      --  +- 1 are here to not Index_Range when rendering
      screen :=
        (Demi_Width            => Demi_Width,
         Demi_Height           => Demi_Height,
         Distance_From_The_Eye => Screen_Distance,
         vision                => vision,
         x                     => 0,
         y                     => 0,
         MIN_X                 => Integer (The_Eye.x) - Demi_Width + 1,
         MAX_X                 => Integer (The_Eye.x) + Demi_Width - 1,
         MIN_Y                 => Integer (The_Eye.y) - Demi_Height + 1,
         MAX_Y                 => Integer (The_Eye.y) + Demi_Height - 1);

      cam :=
        (The_Eye,
         screen,
         l => Demi_Width,
         r => Demi_Width,
         t => Demi_Height,
         b => Demi_Height,
         n => n,
         f => f);

      return cam;

   end Create_Apparatus;

end Camera;
