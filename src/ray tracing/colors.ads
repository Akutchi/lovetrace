package Colors is

   type Color is record

      Red, Green, Blue : Float;
   end record;

   RED        : constant Color := (1.0, 0.0, 0.0);
   PALE_RED   : constant Color := (0.67, 0.0, 0.0);
   DARKER_RED : constant Color := (0.48, 0.0, 0.0);
   BLACK      : constant Color := (0.0, 0.0, 0.0);

end Colors;
