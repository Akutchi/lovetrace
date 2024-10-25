with Ada.Numerics.Generic_Real_Arrays;

with Ada.Text_IO;

package body Tracing is

   type Real is digits 2;

   package Lin_Alg is new Ada.Numerics.Generic_Real_Arrays (Real);
   use Lin_Alg;

   --------------
   -- Init_Ray --
   --------------

   function Init_Ray (o, dir : Vertex; t_min, t_max : Float) return Ray is
      R : Ray;
   begin

      R.o := o;
      R.dir := dir;
      R.t_min := t_min;
      R.t_max := t_max;

      return R;

   end Init_Ray;

   --------------
   -- Value_At --
   --------------

   function Value_At (R : Ray; t : Float) return Vertex is
   begin

      if t < R.t_min then
         return R.o;

      elsif t > R.t_max then
         return R.o + R.t_max * R.dir;

      end if;

      return R.o + t * R.dir;

   end Value_At;

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

   ------------
   -- origin --
   ------------

   function origin (R : Ray) return Vertex is
   begin
      return R.o;
   end origin;

   ---------
   -- dir --
   ---------

   function dir (R : Ray) return Vertex is
   begin
      return R.dir;
   end dir;

   ---------------
   -- Intersect --
   ---------------

   procedure Intersect (Vs : V_List.Vector; R : Ray; H : in out Hit) is

      C : constant Vertex := Vs (1);
      A : constant Vertex := Vs (2);
      B : constant Vertex := Vs (3);

      u_neg : constant Vertex := (-1.0) * dir (R);

      CA : constant Vertex := A - C;
      CB : constant Vertex := B - C;
      CO : constant Vertex := origin (R) - C;

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

      begin

         if a >= 0.0
           and then b >= 0.0
           and then a + b <= 1.0
           and then t_min (R) < t
           and then t_max (R) > t
         then

            H.Touched_Object := True;
            H.t := t;

         end if;
      end;

   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line ("No Solutions !");
         H.t := t_max (R); --  When M is non-invertible

   end Intersect;

   ----------
   -- Cast --
   ----------

   function Cast (R : Ray; Objs : ObjLoader.Scene) return Color is

      H         : Hit;
      Vs        : constant V_List.Vector := Objs.Vertex_List;
      current_t : Float := t_max (R);

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

         Ada.Text_IO.Put_Line (Float'Image (current_t));

      end loop;

      Ada.Text_IO.Put_Line ("---");

      if current_t /= t_max (R) then

         return RED;

      end if;

      return DARKER_RED;

   end Cast;

end Tracing;
