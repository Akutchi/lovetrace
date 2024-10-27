with Ada.Containers; use Ada.Containers;

package body Math.Geometry is

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
              Positive (0.5 * (-3 * J ** 2 + 13 * J - 8));
            F2 : constant Positive :=
              Positive (0.5 * (3 * J ** 2 - 11 * J + 12));
            F3 : constant Positive := 4 - J;

            Anchor : constant Point := Face_Vertices (F1);
            A      : constant Point := Face_Vertices (F2);
            B      : constant Point := Face_Vertices (F3);

            N : constant Point :=
              Normalize (Normal_From_Points (Anchor, B, A));
         begin

            N_List.Append (Face_Normals, N);
         end;
      end loop;

      return Face_Normals;

   end Get_Face_Normals;

end Math.Geometry;
