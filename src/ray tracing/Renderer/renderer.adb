with Ada.Text_IO;

package body Renderer is

   --------------------
   -- Float_To_UInt8 --
   --------------------

   function Float_To_UInt8 (x : Float) return Interfaces.Unsigned_8 is
   begin
      return Interfaces.Unsigned_8 (Float'Rounding (x * 255.0));
   end Float_To_UInt8;

   -------------------------
   -- Color_To_Color_Info --
   -------------------------

   function Color_To_Color_Info (C : Color) return IIO.Color_Info is

      Color_I : IIO.Color_Info;

   begin

      Color_I.Red := Float_To_UInt8 (C.Red);
      Color_I.Green := Float_To_UInt8 (C.Green);
      Color_I.Blue := Float_To_UInt8 (C.Blue);

      return Color_I;

   end Color_To_Color_Info;

   procedure Create_Image (File_Name : String; cam : Camera.Apparatus) is

      Image  : IIO_H.Handle;
      Width  : constant Positive := 2 * cam.screen.Demi_Width;
      Height : constant Positive := 2 * cam.screen.Demi_Height;

   begin

      IIO_H.Create (Image, Width, Height);

      declare
         Data : constant IIO.Image_Data := Image.Value;
      begin
         IIO_O.Write_PNG (Image_Destination & File_Name, Data);
      end;

   end Create_Image;

   procedure Put_Pixel
     (Data : in out IIO.Image_Data; C : Color; cam : Camera.Apparatus)
   is

      X : constant Natural := cam.screen.x - cam.screen.MIN_X;
      Y : constant Natural := -cam.screen.y + cam.screen.MAX_Y;
   begin

      Data (X, Y) := Color_To_Color_Info (C);

   end Put_Pixel;

end Renderer;
