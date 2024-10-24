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

   Screen : constant Camera.Screen_Details :=
     (Demi_Width => 50, Demi_Height => 50, Distance_From_The_Eye => 5,
      vision     => 10, x => 0, y => 0);

   cam : Camera.Apparatus := Camera.Create_Apparatus (o, Screen);

   MIN_X : constant Integer := Integer (o.x) - Integer (cam.l);
   MAX_X : constant Integer := Integer (o.x) + Integer (cam.r) - 1;

   MIN_Y : constant Integer := Integer (o.y) - Integer (cam.b);
   MAX_Y : constant Integer := Integer (o.y) + Integer (cam.t) - 1;

   Image : IIO_H.Handle;

begin

   ObjLoader.Loader ("../scenes/sphere.obj", Objs);
   Renderer.Create_Image ("../scenes_image/res.png", cam);
   IIO_O.Read ("../scenes_image/res.png", Image);

   declare
      Data : IIO.Image_Data := Image.Value;

      non_norm_dir, dir : G.Vertex;
      R                 : Tracing.Ray;
      Pixel_Color       : Colors.Color;

   begin

      for X in MIN_X .. MAX_X loop
         for Y in MIN_Y .. MAX_Y loop

            cam.screen.x := X;
            cam.screen.y := Y;

            non_norm_dir :=
              (Float (cam.screen.x), Float (cam.screen.y), Float (cam.n), 1.0);
            non_norm_dir := G."-" (non_norm_dir, o);
            dir          := G.norm (non_norm_dir);

            R := Tracing.Init_Ray (o, dir, Float (cam.n), Float (cam.f));
            Pixel_Color := Tracing.Cast (R, Objs);

            Renderer.Put_Pixel (Data, Pixel_Color, cam);

         end loop;
      end loop;

      IIO_O.Write_PNG ("../scenes_image/res.png", Data);

   end;

end Lovetrace;
