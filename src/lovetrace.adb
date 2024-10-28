with Image_IO;
with Image_IO.Holders;
with Image_IO.Operations;

with ObjLoader;

with Math;
with Math.Tracing;
with Math.Octree;

with Camera;
with Sources;
with Renderer;
with Colors;

procedure Lovetrace is

   package IIO renames Image_IO;
   package IIO_H renames Image_IO.Holders;
   package IIO_O renames Image_IO.Operations;

   package T renames Math.Tracing;
   package M_O renames Math.Octree;
   package M_OS renames Math.Octree.Octree_Struct;

   Objs : ObjLoader.Scene;

   O_Tree      : M_OS.Tree := M_OS.Empty_Tree;
   First_Child : M_OS.Cursor;

   o     : Math.Point;
   cam   : Camera.Apparatus;
   light : Sources.Point_Source;

   dz : constant Float := 0.01;
   --  This is because, when intersecting a line with a triangle
   --  (Cf. math-tracing.adb) if Z = 0, then the system looks like this :
   --
   --  | CAx  CBx  Ux     |  a    |  Yx
   --  | CAy  CBy  Uy  *  |  b  = |  Yy
   --  | CAz  CBz  0      |  t    |  Yz

   --  making it so that the system cannot be solved for t
   --  (thus, no intersection).

   Image : IIO_H.Handle;

begin

   ObjLoader.Loader ("../scenes/tree_part.obj", Objs);

   M_O.Create_Octree (O_Tree, Objs.Vertex_List, Objs.Faces_List);
   First_Child := M_OS.First_Child (M_OS.Root (O_Tree));

   --  Every vector are put in the "eye coordinates". Aka with o at (0, 0, 0).
   --  I get the barycenter to automatically put the camera in the right place.
   o := ObjLoader.Get_Scene_Barycenter (Objs);
   Camera.Adjust_Origin (o, (0.5, 40.0, 0.5));

   light.origin := (5.0, 5.0, 20.0);

   cam :=
     Camera.Create_Apparatus
       (The_Eye         => o,
        Screen_Distance => 5.0,
        alpha_z         => 0.1,
        alpha_x         => 0.0,
        Vision          => 30.0,
        Demi_Width      => 200,
        Demi_Height     => 200);
   --  vision can be bettered by taking the farthest
   --  barycenter + the object width or smth
   --  to adapt to every scene.

   Renderer.Create_Image ("../scenes_image/res.png", cam);
   IIO_O.Read ("../scenes_image/res.png", Image);

   declare
      Data : IIO.Image_Data := Image.Value;

      unnorm_dir, dir : Math.Point;
      Ray             : T.Ray;
      Pixel_Color     : Colors.Color;

   begin

      for X in cam.screen.MIN_X .. cam.screen.MAX_X - 1 loop
         for Z in reverse cam.screen.MIN_Z + 1 .. cam.screen.MAX_X - 1 loop

            unnorm_dir :=
              (Float (X) / Float (cam.screen.MAX_X),
               -cam.n,
               Float (Z) / Float (cam.screen.MAX_Z) + dz);

            dir := Math.Normalize (unnorm_dir);

            Ray := T.Init_Ray (cam, dir, t_min => 0.0, t_max => cam.f);
            Pixel_Color := Ray.Cast (Objs, First_Child, light);

            cam.screen.x := X;
            cam.screen.z := Z;
            Renderer.Put_Pixel (Data, Pixel_Color, cam);

         end loop;
      end loop;

      IIO_O.Write_PNG ("../scenes_image/res.png", Data);

   end;

end Lovetrace;
