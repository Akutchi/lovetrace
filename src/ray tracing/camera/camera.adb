with Ada.Text_IO;

package body Camera is

   function Create_Apparatus
     (The_Eye                                          : Geometry.Vertex;
      Screen_Distance, Demi_Width, Demi_Height, Vision : Integer)
      return Apparatus
   is

      n : constant Positive := Screen_Distance;
      f : constant Positive := Vision;

      screen : Screen_Details;
      cam    : Apparatus;

   begin

      screen :=
        (Demi_Width            => Demi_Width,
         Demi_Height           => Demi_Height,
         Distance_From_The_Eye => Screen_Distance,
         vision                => Vision,
         x                     => 0,
         y                     => 0,
         MIN_X                 => Integer (The_Eye.x) - Demi_Width,
         MAX_X                 => Integer (The_Eye.x) + Demi_Width,
         MIN_Y                 => Integer (The_Eye.y) - Demi_Height,
         MAX_Y                 => Integer (The_Eye.y) + Demi_Height);

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
