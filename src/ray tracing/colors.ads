package Colors is

   --  from here https://htmlcolorcodes.com/color-names/
   type ColorRep is
     (Red,           --  Red
      DarkRed,
      FireBrick,
      Crimson,
      Green,         --  Green
      DarkGreen,
      ForestGreen,
      Lime,
      Blue,          --  Blue
      DarkBlue,
      DogerBlue,
      LightSkyBlue,
      White,         --  Nuances
      LightGray,
      Gray,
      SlateGray,
      Black);

   type Color is record

      Red, Green, Blue : Float;
   end record;

   function Choose (Color_Str : String) return Color;

end Colors;
