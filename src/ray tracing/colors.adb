package body Colors is

   function Choose (Color_Str : String) return Color is
   begin

      case ColorRep'Value (Color_Str) is

         when Red =>
            return (1.0, 0.0, 0.0);

         when DarkRed =>
            return (0.54, 0.0, 0.0);

         when FireBrick =>
            return (0.70, 0.13, 0.13);

         when Crimson =>
            return (0.86, 0.08, 0.24);

         when Green =>
            return (0.0, 0.5, 0.0);

         when DarkGreen =>
            return (0.0, 0.39, 0.0);

         when ForestGreen =>
            return (0.13, 0.54, 0.13);

         when Lime =>
            return (0.0, 1.0, 0.0);

         when Blue =>
            return (0.0, 0.0, 1.0);

         when DarkBlue =>
            return (0.0, 0.0, 0.54);

         when DogerBlue =>
            return (0.18, 0.56, 1.0);

         when LightSkyBlue =>
            return (0.53, 0.81, 0.98);

         when White =>
            return (1.0, 1.0, 1.0);

         when LightGray =>
            return (0.83, 0.83, 0.83);

         when Gray =>
            return (0.50, 0.50, 0.50);

         when SlateGray =>
            return (0.44, 0.50, 0.56);

         when Black =>
            return (0.0, 0.0, 0.0);

         when NoColor =>
            return (-1.0, -1.0, -1.0);

      end case;

   end Choose;

end Colors;
