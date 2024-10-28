with Ada.Text_IO;

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

   ---------------------------
   -- To_Camera_Coordinates --
   ---------------------------

   function To_Camera_Coordinates (R : Ray; v : Point) return Point is

      v_shift : constant Point := v - R.cam.origin;
   begin

      return Rotate (Rotate (v_shift, 'z', R.cam.alpha_z), 'x', R.cam.alpha_x);

   end To_Camera_Coordinates;

   procedure swap (a, b : in out Float) is
      tmp : Float;
   begin
      tmp := a;
      a := b;
      b := tmp;
   end swap;

   -------------------
   -- Box_Intersect --
   -------------------

   function Box_Intersect
     (R : Ray; Box : M_O.Octree_Struct.Cursor) return Float
   is

      Bounds : constant Real_Vector := M_O.Octree_Struct.Element (Box).Bounds;
      min    : constant Point :=
        (Float (Bounds (1)), Float (Bounds (3)), Float (Bounds (5)));
      max    : constant Point :=
        (Float (Bounds (2)), Float (Bounds (4)), Float (Bounds (6)));

      ε : constant Float := 0.01;

      tmin : Float := (min.x - R.cam.origin.x) / (R.dir.x + ε);
      tmax : Float := (max.x - R.cam.origin.x) / (R.dir.x + ε);

      tymin : constant Float := (min.y - R.cam.origin.y) / (R.dir.y + ε);
      tymax : constant Float := (max.y - R.cam.origin.y) / (R.dir.y + ε);

      tzmin : Float := (min.z - R.cam.origin.z) / (R.dir.z + ε);
      tzmax : Float := (max.z - R.cam.origin.z) / (R.dir.z + ε);

   begin

      if tmin > tmax then
         swap (tmin, tmax);
      end if;

      if (tmin > tymax) or else (tymin > tmax) then
         return -1.0;
      end if;

      if tymin > tmin then
         tmin := tymin;
      end if;

      if tymax < tmax then
         tmax := tymax;
      end if;

      if tymin > tymax then
         swap (tmin, tmax);
      end if;

      if tzmin > tzmax then
         swap (tzmin, tzmax);
      end if;

      if (tmin > tzmax) or else (tzmin > tmax) then
         return -1.0;
      end if;

      if tzmin > tmin then
         tmin := tzmin;
      end if;

      if tzmax < tmax then
         tmax := tzmax;
      end if;

      return (if tmin < tmax then tmin else tmax);

   end Box_Intersect;

   ----------------------
   -- Octree_Intersect --
   ----------------------

   function Octree_Intersect
     (R : Ray; Box : M_O.Octree_Struct.Cursor) return M_O.Octree_Struct.Cursor
   is

      Current_Child, Final_Child : M_O.Octree_Struct.Cursor;
      Child_Nb                   : constant Natural :=
        Natural (M_O.Octree_Struct.Child_Count (Box));
      I                          : Natural := 0;

      t, curr_t : Float := -1.0;

   begin

      if M_O.Octree_Struct.Is_Leaf (Box) then
         return Box;
      end if;

      Current_Child := M_O.Octree_Struct.First_Child (Box);

      while I < Child_Nb loop

         t := Box_Intersect (R, Current_Child);

         if curr_t = -1.0 then
            curr_t := t;
            Final_Child := Current_Child;

         elsif t < curr_t then
            curr_t := t;
            Final_Child := Current_Child;
         end if;

         Current_Child := M_O.Octree_Struct.Next_Sibling (Current_Child);
         I := I + 1;

      end loop;

      return Octree_Intersect (R, Final_Child);

   end Octree_Intersect;

   -----------------------
   -- Point_In_Triangle --
   -----------------------

   function Point_In_Triangle (a, b, ε : Float) return Boolean is
   begin
      return a >= 0.0 and then b >= 0.0 and then a + b <= 1.0 - ε;
   end Point_In_Triangle;

   -----------------
   -- Is_In_Range --
   -----------------

   function Is_In_Range (R : Ray; t : Float) return Boolean is
   begin
      return R.t_min <= t and then t <= R.t_max;
   end Is_In_Range;

   ----------------------
   -- Object_Intersect --
   ----------------------

   procedure Object_Intersect
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
         ε : constant Float := 0.001;

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

   end Object_Intersect;

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
     (R         : Ray;
      Objs      : ObjLoader.Scene;
      Tree_Root : M_O.Octree_Struct.Cursor;
      Light     : Sources.Abstract_Source'Class) return Color
   is

      Vs : constant V_List.Vector := Objs.Vertex_List;
      Ns : constant N_List.Vector := Objs.Normal_List;

      H               : Hit;
      Current_t       : Float := -1.0;
      Normal_On_Touch : Point;

      Box : M_O.Octree_Struct.Cursor;

   begin

      Box := Octree_Intersect (R, Tree_Root);

      for F of M_O.Octree_Struct.Element (Box).Fi loop

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

            R.Object_Intersect (Face_Vertices, Face_Normals, H);

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
