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
        Demi_Width      => 50,
        Demi_Height     => 50,
        vision          => 800);
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

      no_norm_dir, dir : G.Vertex;
      R                : Tracing.Ray;
      Pixel_Color      : Colors.Color;

   begin

      for X in cam.screen.MIN_X .. cam.screen.MAX_X loop
         for Y in cam.screen.MIN_Y .. cam.screen.MAX_Y loop

            cam.screen.x := X;
            cam.screen.y := Y;

            no_norm_dir :=
              (Float (cam.screen.x), Float (cam.screen.y), Float (cam.n), 1.0);
            no_norm_dir := G."-" (no_norm_dir, o);
            dir := G.norm (no_norm_dir);

            R := Tracing.Init_Ray (o, dir, 0.0, Float (cam.f));
            --  can be a bit optimized by starting t from screen instead of
            --  The Eye

            Pixel_Color := Tracing.Cast (R, Objs);

            Renderer.Put_Pixel (Data, Pixel_Color, cam);

         end loop;
      end loop;

      IIO_O.Write_PNG ("../scenes_image/res.png", Data);

   end;

end Lovetrace;
