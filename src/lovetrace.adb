with ObjLoader;
with Geometry;
with Camera;
with Renderer;

procedure Lovetrace is

   Objs : ObjLoader.Scene;

   w : constant Float           := 1.0;
   o : constant Geometry.Vertex := (0.0, 0.0, -10.0, w);

   Screen : constant Camera.Screen_Details :=
     (top_right   => (100.0, 100.0, -5.0, w),
      bottom_left => (-100.0, -100.0, -5.0, w), vision => 10, x => 0, y => 0);

   cam : constant Camera.Apparatus := Camera.Create_Apparatus (o, Screen);

begin

   ObjLoader.Loader ("../scenes/sphere.obj", Objs);
   Renderer.Create_Image ("../scenes_image/res.png", cam);

end Lovetrace;
