with Interfaces;

with Image_IO;
with Image_IO.Holders;
with Image_IO.Operations;

with Camera;

package Renderer is

   package IIO renames Image_IO;
   package IIO_H renames Image_IO.Holders;
   package IIO_O renames Image_IO.Operations;

   Image_Destination : constant String := "../scenes_image/";

   type Color is record

      Red, Green, Blue : Float;
   end record;

   RED : constant Color := (1.0, 0.0, 0.0);

   function Float_To_UInt8 (x : Float) return Interfaces.Unsigned_8;

   function Color_To_Color_Info (C : Color) return IIO.Color_Info;

   procedure Create_Image (File_Name : String; cam : Camera.Apparatus);

   procedure Put_Pixel
     (Data : in out IIO.Image_Data; C : Color; cam : Camera.Apparatus);
   --  here (0, 0) is in the top left hand corner

end Renderer;
