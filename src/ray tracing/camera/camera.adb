package body Camera is

   function Create_Apparatus
     (o : Geometry.Vertex; Screen : Screen_Details) return Apparatus
   is

      l : constant Integer := Integer (Screen.top_right.x) / 2;
      r : constant Integer := Integer (Screen.top_right.x) - l;
      t : constant Integer := Integer (Screen.bottom_left.y) / 2;
      b : constant Integer := Integer (Screen.bottom_left.y) - t;

      n : constant Integer := Integer (Screen.top_right.z);
      f : constant Integer := n - Screen.vision;

   begin

      return (o, Screen, l, r, t, b, n, f);

   end Create_Apparatus;

end Camera;
