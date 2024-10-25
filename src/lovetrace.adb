with Image_IO;
with Image_IO.Holders;
with Image_IO.Operations;

with ObjLoader;
with Geometry;
with Camera;
with Renderer;
with Tracing;
with Colors;

procedure Lovetrace is

   package IIO renames Image_IO;
   package IIO_H renames Image_IO.Holders;
   package IIO_O renames Image_IO.Operations;

   package G renames Geometry;

   Objs : ObjLoader.Scene;

   o : constant G.Vertex := (0.0, 0.0, 10.0, 1.0);

   cam : Camera.Apparatus :=
     Camera.Create_Apparatus
       (The_Eye         => o,
        Screen_Distance => 5,
        Demi_Width      => 100,
        Demi_Height     => 100,
        vision          => 20);
   --  vision can be bettered by taking the farthest
   --  barycenter + the object width (or smth)
   --  to adapt to every scene.

   Image : IIO_H.Handle;

begin

   ObjLoader.Loader ("../scenes/sphere.obj", Objs);
   Renderer.Create_Image ("../scenes_image/res.png", cam);
   IIO_O.Read ("../scenes_image/res.png", Image);

   declare
      Data : IIO.Image_Data := Image.Value;

      unnorm_dir, dir : G.Vertex; --  in eye coordinates (at the origin)
      R               : Tracing.Ray;
      Pixel_Color     : Colors.Color;

   begin

      for X in cam.screen.MIN_X .. cam.screen.MAX_X - 1 loop
         for Y in reverse cam.screen.MIN_Y + 1 .. cam.screen.MAX_Y loop

            unnorm_dir := (Float (X), Float (Y), Float (-cam.n), 1.0);
            dir := G.norm (unnorm_dir);

            R :=
              Tracing.Init_Ray
                (o, dir, t_min => Float (cam.n), t_max => Float (cam.f));
            Pixel_Color := R.Cast (Objs);

            cam.screen.x := X;
            cam.screen.y := Y;
            Renderer.Put_Pixel (Data, Pixel_Color, cam);

         end loop;
      end loop;

      IIO_O.Write_PNG ("../scenes_image/res.png", Data);

   end;

end Lovetrace;
