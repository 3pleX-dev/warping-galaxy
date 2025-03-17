open Claudius

type point = { x : float; y : float; z : float }

let rotate_z (a : float) (p : point) : point =
  { p with
    x = (p.x *. cos a) -. (p.y *. sin a);
    y = (p.x *. sin a) +. (p.y *. cos a);
  }

let generate_galaxy (ft : float) : point list =
  let num_spirals = 5  
  and num_stars = 1000  
  and spiral_factor = 0.2 in  
  
  List.init num_stars (fun i ->
    let fi = float_of_int i in
    let arm_index = i mod num_spirals in  
    let base_angle = (float_of_int arm_index) *. (Float.pi *. 2. /. float_of_int num_spirals) in
    let angle = base_angle +. (spiral_factor *. fi) in
    let radius = fi *. 0.15 +. 10. in
    let depth = mod_float fi 100. in
    {
      x = radius *. cos angle;
      y = radius *. sin angle;
      z = depth;
    }
    |> rotate_z (ft *. 0.005)  
  )

let render_to_primitives (ft : float) (s : Screen.t) (points : point list) : Primitives.t list =
  let width, height = Screen.dimensions s in
  let m = 1000. +. (cos (ft /. 40.) *. 500.) in
  List.map (fun e ->
    let depth_color = int_of_float ((e.z /. 100.) *. 15.) in
    Primitives.Pixel ({
      x = (width / 2) + int_of_float (m *. e.x /. (e.z +. 200.));
      y = (height / 2) + int_of_float (m *. e.y /. (e.z +. 200.));
    }, depth_color)
  ) points

let tick (t : int) (s : Screen.t) (prev : Framebuffer.t) (_inputs : Base.KeyCodeSet.t): Framebuffer.t =
  let buffer = Framebuffer.map (fun pixel -> if pixel > 1 then pixel - 1 else 0) prev in
  let ft = float_of_int t in
  generate_galaxy ft
  |> render_to_primitives ft s
  |> Framebuffer.render buffer;
  buffer

let () =
  Screen.create 640 480 1 (Palette.generate_mono_palette 16) |> Base.run "Warping Galaxy" None tick
