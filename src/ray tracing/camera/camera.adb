package body Camera is

   ----------------------
   -- Create_Apparatus --
   ----------------------

   function Create_Apparatus
     (The_Eye                                   : Math.Point;
      Screen_Distance, Vision, alpha_z, alpha_x : Float;
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
         z                     => 0,
         MIN_X                 => Integer (The_Eye.x) - Demi_Width,
         MAX_X                 => Integer (The_Eye.x) + Demi_Width,
         MIN_Z                 => Integer (The_Eye.z) - Demi_Height,
         MAX_Z                 => Integer (The_Eye.z) + Demi_Height);

      cam :=
        (The_Eye,
         screen,
         l       => Demi_Width,
         r       => Demi_Width,
         t       => Demi_Height,
         b       => Demi_Height,
         n       => n,
         f       => f,
         alpha_z => alpha_z,
         alpha_x => alpha_x);

      return cam;

   end Create_Apparatus;

   -------------------
   -- Adjust_Origin --
   -------------------

   procedure Adjust_Origin (o : in out Math.Point; v : Math.Point) is
   begin
      o := Math."+" (o, v);
   end Adjust_Origin;

end Camera;
