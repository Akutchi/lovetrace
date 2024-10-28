package body Math.Tracing is

   --------------
   -- Init_Ray --
   --------------

   function Init_Ray
     (cam : Camera.Apparatus; dir : Point; t_min, t_max : Float) return Ray
   is
      R : Ray;
   begin

      R.cam := cam;
      R.dir := dir;
      R.t_min := t_min;
      R.t_max := t_max;

      return R;

   end Init_Ray;

   -----------------
   -- Is_In_Range --
   -----------------

   function Is_In_Range (R : Ray; t : Float) return Boolean is
   begin
      return R.t_min < t and then t < R.t_max;
   end Is_In_Range;

   ---------------------------
   -- To_Camera_Coordinates --
   ---------------------------

   function To_Camera_Coordinates (R : Ray; v : Point) return Point is

      v_shift : constant Point := v - R.cam.origin;
   begin

      return Rotate (Rotate (v_shift, 'y', R.cam.alpha_y), 'x', R.cam.alpha_x);

   end To_Camera_Coordinates;

   -----------------------
   -- Point_In_Triangle --
   -----------------------

   function Point_In_Triangle (a, b, ε : Float) return Boolean is
   begin
      return a >= 0.0 and then b >= 0.0 and then a + b <= 1.0 - ε;
   end Point_In_Triangle;

   ---------------
   -- Intersect --
   ---------------

   procedure Intersect
     (R : Ray; Vs : V_List.Vector; Ns : N_List.Vector; H : in out Hit)
   is

      C : constant Point := R.To_Camera_Coordinates (Vs (1));
      A : constant Point := R.To_Camera_Coordinates (Vs (2));
      B : constant Point := R.To_Camera_Coordinates (Vs (3));

      N1 : constant Point := R.To_Camera_Coordinates (Ns (1));
      N2 : constant Point := R.To_Camera_Coordinates (Ns (2));
      N3 : constant Point := R.To_Camera_Coordinates (Ns (3));

      u_neg : constant Point := (-1.0) * R.dir;

      CA : constant Point := A - C;
      CB : constant Point := B - C;
      CO : constant Point := (-1.0) * C;

      M : constant Real_Matrix :=
        ((Real (CA.x), Real (CB.x), Real (u_neg.x)),
         (Real (CA.y), Real (CB.y), Real (u_neg.y)),
         (Real (CA.z), Real (CB.z), Real (u_neg.z)));

      Y : constant Real_Vector := (Real (CO.x), Real (CO.y), Real (CO.z));
   begin

      declare

         Sol : constant Real_Vector := Lin_Alg.Solve (M, Y);

         a : constant Float := Float (Sol (Sol'First));
         b : constant Float := Float (Sol (Sol'First + 1));
         t : constant Float := Float (Sol (Sol'First + 2));
         ε : constant Float := 0.01;

      begin

         if Point_In_Triangle (a, b, ε) and then R.Is_In_Range (t) then

            H.Touched_Object := True;
            H.t := t;
            H.Normal_On_Touch :=
              Normalize (N2 + a * (N3 - N2) + b * (N1 - N2));

         end if;
      end;

   exception
      when Constraint_Error =>
         H.t := H.NO_INTERSECTION; --  When M is non-invertible, no solutions

   end Intersect;

   --------------
   -- Value_At --
   --------------

   function Value_At
     (R : Ray; t : Float; Ray_From_Camera : Boolean) return Point
   is

      o : constant Point :=
        (if Ray_From_Camera then (0.0, 0.0, 0.0) else R.cam.origin);
   begin

      if t > R.t_max then
         return o + R.t_max * R.dir;

      elsif t < R.t_min then
         return o + R.t_min * R.dir;
      end if;

      return o + t * R.dir;

   end Value_At;

   ---------------------
   -- Light_Intensity --
   ---------------------

   function Light_Intensity
     (R : Ray; at_P, N : Point; Light : Sources.Abstract_Source'Class)
      return Color
   is

      I : constant Color := Choose ("Red");
      L : constant Point := Normalize (at_P - Light.origin);
      η : constant Float := 1.0;

      dempening : constant Float := η * Float'Max (0.0, L * N);

   begin

      return (dempening * I.Red, dempening * I.Green, dempening * I.Blue);

   end Light_Intensity;

   ----------
   -- Cast --
   ----------

   function Cast
     (R : Ray; Objs : ObjLoader.Scene; Light : Sources.Abstract_Source'Class)
      return Color
   is

      Vs : constant V_List.Vector := Objs.Vertex_List;
      Ns : constant N_List.Vector := Objs.Normal_List;

      H               : Hit;
      Current_t       : Float := -1.0;
      Normal_On_Touch : Point;

   begin

      for F of Objs.Faces_List loop

         declare
            V_Indices     : constant Indices_List := F.Vertices_Indices;
            Face_Vertices : V_List.Vector;

            N_Indices    : constant Indices_List := F.Normals_Indices;
            Face_Normals : N_List.Vector;

         begin

            V_List.Append (Face_Vertices, Vs (V_Indices (1)));
            V_List.Append (Face_Vertices, Vs (V_Indices (2)));
            V_List.Append (Face_Vertices, Vs (V_Indices (3)));

            Face_Normals := Get_Face_Normals (Ns, N_Indices, Face_Vertices);

            R.Intersect (Face_Vertices, Face_Normals, H);

            if Current_t = -1.0 then
               Current_t := H.t;
               Normal_On_Touch := H.Normal_On_Touch;

            elsif H.t < Current_t then
               Current_t := H.t;
               Normal_On_Touch := H.Normal_On_Touch;

            end if;
         end;
      end loop;

      if Current_t /= H.NO_INTERSECTION then

         return
           R.Light_Intensity
             (R.Value_At (Current_t, Ray_From_Camera => True),
              Normal_On_Touch,
              Light);

      end if;

      return Choose ("Black");

   end Cast;

end Math.Tracing;