with Image_IO;
with Image_IO.Holders;
with Image_IO.Operations;

with ObjLoader;

with Math.Geometry;
with Math.Tracing;

with Camera;
with Sources;
with Renderer;
with Colors;

procedure Lovetrace is

   package IIO renames Image_IO;
   package IIO_H renames Image_IO.Holders;
   package IIO_O renames Image_IO.Operations;

   package G renames Math.Geometry;
   package T renames Math.Tracing;

   Objs : ObjLoader.Scene;

   o : constant G.Vertex := (0.0, 0.0, 10.0, 1.0);

   cam : Camera.Apparatus :=
     Camera.Create_Apparatus
       (The_Eye         => o,
        Screen_Distance => 5.0,
        alpha_y         => 0.0,
        alpha_x         => 0.0,
        Vision          => 30.0,
        Demi_Width      => 200,
        Demi_Height     => 200);
   --  vision can be bettered by taking the farthest
   --  barycenter + the object width or smth
   --  to adapt to every scene.

   light : Sources.Point_Source;

   Image : IIO_H.Handle;

begin

   ObjLoader.Loader ("../scenes/pyramid.obj", Objs);

   Renderer.Create_Image ("../scenes_image/res.png", cam);
   IIO_O.Read ("../scenes_image/res.png", Image);

   declare
      Data : IIO.Image_Data := Image.Value;

      unnorm_dir, dir : G.Vertex; --  in eye coordinates
      Ray             : T.Ray;
      Pixel_Color     : Colors.Color;

   begin

      light.origin := (0.0, 20.0, 5.0, 1.0); --  in eye coordinates

      for X in cam.screen.MIN_X .. cam.screen.MAX_X - 1 loop
         for Y in reverse cam.screen.MIN_Y + 1 .. cam.screen.MAX_Y loop

            unnorm_dir :=
              (Float (X) / Float (cam.screen.MAX_X),
               Float (Y) / Float (cam.screen.MAX_Y),
               -cam.n,
               1.0);

            dir := G.Normalize (unnorm_dir);

            Ray := T.Init_Ray (cam, dir, t_min => 0.0, t_max => cam.f);
            Pixel_Color := Ray.Cast (Objs, light);

            cam.screen.x := X;
            cam.screen.y := Y;
            Renderer.Put_Pixel (Data, Pixel_Color, cam);

         end loop;
      end loop;

      IIO_O.Write_PNG ("../scenes_image/res.png", Data);

   end;

end Lovetrace;
