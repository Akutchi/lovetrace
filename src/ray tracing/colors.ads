package Colors is

   type Color is record

      Red, Green, Blue : Float;
   end record;

   RED   : constant Color := (1.0, 0.0, 0.0);
   BLACK : constant Color := (0.0, 0.0, 0.0);

end Colors;
