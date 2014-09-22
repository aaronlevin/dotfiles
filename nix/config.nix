{
 packageOverrides = pkgs : with pkgs; {
   hackEnv = pkgs.myEnvFun {
       name = "hack";
       buildInputs = [ stdenv SDL SDL_image SDL_ttf SDL_gfx cmake SDL_net pkgconfig ];
   };
 };
}
