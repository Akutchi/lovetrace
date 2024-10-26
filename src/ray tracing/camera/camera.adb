package body Camera is

   function Create_Apparatus
     (The_Eye                                   : Geometry.Vertex;
      Screen_Distance, Vision, alpha_y, alpha_x : Float;
      Demi_Width, Demi_Height                   : Integer) return Apparatus
   is

      n : constant Float := Screen_Distance;
      f : constant Float := Vision;

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
         l       => Demi_Width,
         r       => Demi_Width,
         t       => Demi_Height,
         b       => Demi_Height,
         n       => n,
         f       => f,
         alpha_y => alpha_y,
         alpha_x => alpha_x);

      return cam;

   end Create_Apparatus;

end Camera;
