package body Camera is

   function Create_Apparatus
     (o : Geometry.Vertex; Screen : Screen_Details) return Apparatus
   is

      n : constant Positive :=
        Integer (o.z) - Integer (Screen.Distance_From_The_Eye);

      f : constant Positive := abs (Integer (o.z) - Integer (Screen.vision));

   begin

      return
        (o, Screen, l => Screen.Demi_Width, r => Screen.Demi_Width,
         t            => Screen.Demi_Height, b => Screen.Demi_Height,
         n            => Screen.Distance_From_The_Eye, f => f);

   end Create_Apparatus;

end Camera;
