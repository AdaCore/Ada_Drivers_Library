
with Interfaces; use Interfaces;

package Textures is

   type Texture is array (0 .. 63, 0 .. 63) of Unsigned_16
     with Component_Size => 16;

   type Texture_Access is access constant Texture;

end Textures;
