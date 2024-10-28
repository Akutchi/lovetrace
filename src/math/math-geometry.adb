with Ada.Containers; use Ada.Containers;

package body Math.Geometry is

   ------------------------
   -- Normal_From_Points --
   ------------------------

   function Normal_From_Points (Anchor, A, B : Point) return Point is

      AnA : constant Point := A - Anchor;
      AnB : constant Point := B - Anchor;
   begin

      return
        (AnA.y * AnB.z - AnA.z * AnB.y,
         -(AnA.x * AnB.z - AnA.z * AnB.x),
         (AnA.x * AnB.y - AnA.y * AnB.x));

   end Normal_From_Points;

   ----------------------
   -- Get_Face_Normals --
   ----------------------

   function Get_Face_Normals
     (Ns            : N_List.Vector;
      N_Indices     : Indices_List;
      Face_Vertices : V_List.Vector) return N_List.Vector
   is

      Face_Normals : N_List.Vector;
   begin

      if N_List.Length (Ns) > 0 then

         N_List.Append (Face_Normals, Ns (N_Indices (1)));
         N_List.Append (Face_Normals, Ns (N_Indices (2)));
         N_List.Append (Face_Normals, Ns (N_Indices (3)));

         return Face_Normals;
      end if;

      for J
        in V_List.First_Index (Face_Vertices)
        .. V_List.Last_Index (Face_Vertices)
      loop

         declare

            F1 : constant Positive :=
              Positive ((-3 * J ** 2 + 13 * J - 8) / 2);
            F2 : constant Positive :=
              Positive ((3 * J ** 2 - 11 * J + 12) / 2);
            F3 : constant Positive := 4 - J;

            Anchor : constant Point := Face_Vertices (F1);
            A      : constant Point := Face_Vertices (F2);
            B      : constant Point := Face_Vertices (F3);
            --  Inversed so that the right hand rule is respected. a.k.a
            --  The Normal in pointing "up".

            N : constant Point :=
              Normalize (Normal_From_Points (Anchor, A, B));
         begin

            N_List.Append (Face_Normals, N);
         end;
      end loop;

      return Face_Normals;

   end Get_Face_Normals;

end Math.Geometry;
