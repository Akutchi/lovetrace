with Math.Geometry;

package Sources is

   type Abstract_Source is abstract tagged record
      origin : Math.Geometry.Vertex;

   end record;

   type Point_Source is new Abstract_Source with null record;

end Sources;
