with Ada.Text_IO;

package body Tracing is

   --------------
   -- Init_Ray --
   --------------

   function Init_Ray (o, dir : Vertex; t_min, t_max : Float) return Ray is
      R : Ray;
   begin

      R.origin := o;
      R.dir := dir;
      R.t_min := t_min;
      R.t_max := t_max;

      return R;

   end Init_Ray;

   -----------
   -- t_min --
   -----------

   function t_min (R : Ray) return Float is
   begin
      return R.t_min;
   end t_min;

   -----------
   -- t_max --
   -----------

   function t_max (R : Ray) return Float is
   begin
      return R.t_max;
   end t_max;

   ---------
   -- dir --
   ---------

   function dir (R : Ray) return Vertex is
   begin
      return R.dir;
   end dir;

   ---------------------------
   -- To_Camera_Coordinates --
   ---------------------------

   function To_Camera_Coordinates (R : Ray; v : Vertex) return Vertex is

      v_prime : Vertex := v + R.origin;
   begin

      v_prime.z := -1.0 * v_prime.z; --  z axis is still in the same direction

      return v_prime;

   end To_Camera_Coordinates;

   function To_Camera_Coordinates (R : Ray; v : Real_Vector) return Real_Vector
   is

      v_prime : Real_Vector := v;
   begin

      v_prime (v_prime'First) := v_prime (v_prime'First) + Real (R.origin.x);
      v_prime (v_prime'First + 1) :=
        v_prime (v_prime'First + 1) + Real (R.origin.y);
      v_prime (v_prime'First + 2) :=
        -(v_prime (v_prime'First + 2) + Real (R.origin.z));

      return v_prime;

   end To_Camera_Coordinates;

   ---------------
   -- Intersect --
   ---------------

   procedure Intersect (Vs : V_List.Vector; R : Ray; H : in out Hit) is

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

         if a >= 0.0
           and then b >= 0.0
           and then a + b <= 1.0 - ε
           and then t > R.t_min
           and then t < R.t_max
         then

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
      current_t : Float := R.t_max;

   begin

      for F of Objs.Faces_List loop

         declare
            Indices : constant Indices_List := F.Vertices_Indices;

            Face_Vertices : V_List.Vector;

         begin

            V_List.Append (Face_Vertices, Vs (Indices (1)));
            V_List.Append (Face_Vertices, Vs (Indices (2)));
            V_List.Append (Face_Vertices, Vs (Indices (3)));

            Intersect (Face_Vertices, R, H);

            if H.t < current_t then
               current_t := H.t;

            end if;
         end;

      end loop;

      if current_t /= H.NO_INTERSECTION then

         return RED;

      end if;

      return BLACK;

   end Cast;

end Tracing;
