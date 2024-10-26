with Ada.Text_IO;

package body Math.Tracing is

   --------------
   -- Init_Ray --
   --------------

   function Init_Ray
     (cam : Camera.Apparatus; dir : Vertex; t_min, t_max : Float) return Ray
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

   function To_Camera_Coordinates (R : Ray; v : Vertex) return Vertex is

      v_shift : constant Vertex := v - R.cam.origin;
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

   procedure Intersect (R : Ray; Vs : V_List.Vector; H : in out Hit) is

      C : constant Vertex := R.To_Camera_Coordinates (Vs (1));
      A : constant Vertex := R.To_Camera_Coordinates (Vs (2));
      B : constant Vertex := R.To_Camera_Coordinates (Vs (3));

      u_neg : constant Vertex := (-1.0) * R.dir;

      CA : constant Vertex := A - C;
      CB : constant Vertex := B - C;
      CO : constant Vertex := (-1.0) * C;

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

         end if;
      end;

   exception
      when Constraint_Error =>
         H.t := H.NO_INTERSECTION; --  When M is non-invertible, no solutions

   end Intersect;

   ----------
   -- Cast --
   ----------

   function Cast (R : Ray; Objs : ObjLoader.Scene) return Color is

      H         : Hit;
      Vs        : constant V_List.Vector := Objs.Vertex_List;
      current_t : Float := -1.0;

   begin

      for F of Objs.Faces_List loop

         declare
            Indices       : constant Indices_List := F.Vertices_Indices;
            Face_Vertices : V_List.Vector;

         begin

            V_List.Append (Face_Vertices, Vs (Indices (1)));
            V_List.Append (Face_Vertices, Vs (Indices (2)));
            V_List.Append (Face_Vertices, Vs (Indices (3)));

            R.Intersect (Face_Vertices, H);

            if current_t = -1.0 then
               current_t := H.t;

            elsif H.t < current_t then
               current_t := H.t;

            end if;
         end;

      end loop;

      if current_t /= H.NO_INTERSECTION then

         return Choose ("red");

      end if;

      return Choose ("black");

   end Cast;

end Math.Tracing;
