type parameters_t = {
  length1: float;
  radius1: float;
  epsilon: float;
  area1: float;
  q_pts: int;
  quads: int;
  dist: float;
  n_pts: int;
  k_pts: int;
  a_pts: int;
  b_pts: int;
  c_pts: int;
  d_pts: int;
  e_pts: int;
  f_pts: int;
  g_pts: int;
  length2: float;
  radius2: float;
  area2: float;
}

type point = {
  x: float;
  y: float;
}

let pi = 3.1415926535

let cmp_float f1 f2 =
  if (abs_float (f1 -. f2) < 0.000001) then 0
  else
    if f1 < f2 then -1
    else 1

open Yojson.Basic.Util

let fname = Sys.argv.(1)

(* create the JSON object that represents all the file *)
let json = Yojson.Basic.from_file fname
let seed = json |> member "random_seed" |> to_int
(* get the json object corresponding to the parameters *)
let params = json |> member "PARAMETERS"
(* get the number of points *)
let numpoints = json |> member "NUMPOINTS" |> to_int
(* get the list of points *)
let points = json |> member "points" |> to_list
(* get PUV *)
let puv = json |> member "PUV" |> to_list
(* get LCM *)
let lcm = json |> member "LCM"

let get_someone jsonobj m f =
  jsonobj |> member m |> f

let get_params m f =
  get_someone params m f

let pr_float f =
  Format.printf "%f@." f

let pr_int i =
  Format.printf "%d@." i

(* create the record for PARAMETERS *)
let params =
  {
    radius1 = get_params "RADIUS1" to_float;
    quads = get_params "QUADS" to_int;
    radius2 = get_params "RADIUS2" to_float;
    dist = get_params "DIST" to_float;
    f_pts = get_params "F_PTS" to_int;
    k_pts = get_params "K_PTS" to_int;
    epsilon = get_params "EPSILON" to_float;
    length1 = get_params "LENGTH1" to_float;
    g_pts = get_params "G_PTS" to_int;
    q_pts = get_params "Q_PTS" to_int;
    n_pts = get_params "N_PTS" to_int;
    c_pts = get_params "C_PTS" to_int;
    area2 = get_params "AREA2" to_float;
    length2 = get_params "LENGTH2" to_float;
    b_pts = get_params "B_PTS" to_int;
    d_pts = get_params "D_PTS" to_int;
    area1 = get_params "AREA1" to_float;
    a_pts = get_params "A_PTS" to_int;
    e_pts = get_params "E_PTS" to_int;
  }

let points =
  (* get a point list *)
  let points = List.map (fun p -> match p |> to_list with
                                  | [x; y] -> { x = x |> to_float;
                                                y = y |> to_float }
                                  | _ -> assert false)  points in
  Array.of_list points

let puv =
  (* get a bool list *)
  let puv = List.map (fun b -> b |> to_bool) puv in
  Array.of_list puv

let lcm =
  let lcm_matrix = Array.make_matrix 15 15 (fun _ -> assert false) in
  for i = 0 to 14 do
    (* get an json array *)
    let a = lcm |> member (string_of_int i) |> to_list in
    let a = Array.of_list a in
    (* traverse this array to fill the matrix *)
    for j = 0 to 14 do
      (* get connector *)
      let con = a.(j) |> to_string in
      let fcon =
        match con with
        | "ANDD" -> (fun a b -> a && b)
        | "ORR" -> (fun a b -> a || b)
        | "NOTUSED" -> (fun _ _ -> true)
        | _ -> assert false
      in
      lcm_matrix.(i).(j) <- fcon
    done
  done;
  lcm_matrix

let dist p1 p2 =
  let x = (p2.x -. p1.x) *. (p2.x -. p1.x) in
  let y = (p2.y -. p1.y) *. (p2.y -. p1.y) in
  sqrt (x +. y)

let cmp_points p1 p2 =
  cmp_float p1.x p2.x = 0 && cmp_float p1.y p2.y = 0

let triangle_area p1 p2 p3 =
  let x1 = p1.x in
  let x2 = p2.x in
  let x3 = p3.x in
  let y1 = p1.y in
  let y2 = p2.y in
  let y3 = p3.y in
  let area1 = x1*.(y2 -. y3) +. x2*.(y3 -. y1) +. x3*.(y1 -. y2) in
  let area2 = abs_float area1 in
  area2 /. 2.0

let dist_point_line p1 p2 p3 =
  let numer = 2.0 *. (triangle_area p1 p2 p3) in
  let denum = dist p2 p3 in
  numer /. denum

let cmv = Array.make 15 true

let lic0 =
  let rec loop i top =
    if i >= top then false
    else
      if cmp_float (dist points.(i) points.(i+1)) params.length1 = 1 then
        true
      else
        loop (i+1) top
  in
  cmv.(0) <- loop 0 (numpoints - 1)

let lic1 =
  let rec loop i top =
    if i >= top then
      false
    else
      let a = points.(i) in
      let b = points.(i+1) in
      let c = points.(i+2) in
      let dist_bc = dist b c in
      let dist_ac = dist a c in
      let dist_ab = dist a b in
      let area = triangle_area a b c in
      let abc = dist_bc *. dist_ac *. dist_ab in
      let r2 = abc /. (2.0*.area) in
      let r = r2 /. 2.0 in
      if cmp_float r params.radius1 = 1 then
        true
      else
        loop (i+1) top
  in
  cmv.(1) <- loop 0 (numpoints - 2)

let lic2 =
  let rec loop i top =
    if i >= top then false
    else
      let a = points.(i) in
      let b = points.(i+1) in
      let c = points.(i+2) in
      if cmp_points a b && cmp_points c b then
        false
      else
        let dist_ac = dist a c in
        let dist_ab = dist a b in
        let dist_bc = dist b c in
        let res1 = -.(dist_ac**2.0)+.(dist_ab**2.0)+.(dist_bc**2.0) in
        let res2 = res1 /. (2.0 *. dist_ab *.dist_bc) in
        let res = acos res2 in
        if cmp_float res (pi -. params.epsilon) = -1 ||
           cmp_float res (pi +. params.epsilon) = 1 then
          true
        else
          loop (i+1) top
  in
  cmv.(2) <- loop 0 (numpoints - 2)

let lic3 =
  let rec loop i top =
    if i >= top then false
    else
      let area = triangle_area points.(i) points.(i+1) points.(i+2) in
      if cmp_float area params.area1 = 1 then
        true
      else
        loop (i+1) top
  in
  cmv.(3) <- loop 0 (numpoints - 2)

let lic4 =
  let rec loop i top =
    if i >= top then
      false
    else
      let a = Array.make 4 0 in
      let c_quad = ref 0 in
      let update i =
        if a.(i) = 0 then begin
            a.(i) <- 1;
            incr c_quad
          end
      in
      for j = i to i + params.q_pts do
        let p = points.(j) in
        if cmp_float p.x 0.0 >= 0 then
          if cmp_float p.y 0.0 >= 0 then
            (* first *)
            update 0
          else
            (* fourth *)
            update 3
        else
          if cmp_float p.y 0.0 >= 0 then
            (* second *)
            update 1
          else
            (* third *)
            update 2
      done;
      if !c_quad > params.quads then
        true
      else
        loop (i+1) top
  in
  cmv.(4) <- loop 0 (numpoints - params.q_pts)

let lic5 =
  let rec loop i top =
    if i >= top then false
    else
      if cmp_float points.(i+1).x points.(i).x = -1 then
        true
      else
        loop (i+1) top
  in
  cmv.(5) <- loop 0 (numpoints - 1)

let lic6 =
  let rec loop i top =
    if i >= top then false
    else
      let p1 = points.(i) in
      let p2 = points.(i+params.n_pts) in
      if cmp_points p1 p2 then
        let rec loop2 j top2 =
          if j >= top2 then
            false
          else
            let p = points.(j) in
            let d = dist p p1 in
            if cmp_float d params.dist = 1 then
              true
            else
              loop2 (j+1) top2
        in
        loop2 i (i+params.n_pts)
      else
        let rec loop2 j top2 =
          if j >= top2 then
            false
          else
            let p = points.(j) in
            let d = dist_point_line p p1 p2 in
            if cmp_float d params.dist = 1 then
              true
            else
              loop2 (j+1) top2
        in
        loop2 i (i+params.n_pts)
  in
  let res =
    if numpoints < 3 then false
    else loop 0 (numpoints - params.n_pts)
  in
  cmv.(6) <- res

let lic7 =
  let rec loop i top =
    if i >= top then false
    else
      let p1 = points.(i) in
      let p2 = points.(i+params.k_pts) in
      let dist = dist p1 p2 in
      if cmp_float dist params.length1 = 1 then
        true
      else
        loop (i+1) top
  in
  let res =
    if numpoints < 3 then false
    else
      loop 0 (numpoints - params.k_pts)
  in
  cmv.(7) <- res

let lic8 =
  let rec loop i top =
    if i >= top then false
    else
      let a = points.(i) in
      let b = points.(i+params.a_pts) in
      let c = points.(i+params.a_pts+params.b_pts) in
      let dist_bc = dist b c in
      let dist_ac = dist a c in
      let dist_ab = dist a b in
      let area = triangle_area a b c in
      let abc = dist_bc *. dist_ac *. dist_ab in
      let r2 = abc /. (2.0*.area) in
      let r = r2 /. 2.0 in
      if cmp_float r params.radius1 = 1 then
        true
      else
        loop (i+1) top
  in
  let res =
    if numpoints < 5 then
      false
    else
      loop 0 (numpoints - (params.a_pts+params.b_pts))
  in
  cmv.(8) <- res

let lic9 =
  let rec loop i top =
    if i >= top then
      false
    else
      let a = points.(i) in
      let b = points.(i+params.c_pts) in
      let c = points.(i+params.c_pts+params.d_pts) in
      if cmp_points a b && cmp_points c b then
        false
      else
        let dist_ac = dist a c in
        let dist_ab = dist a b in
        let dist_bc = dist b c in
        let res1 = -.(dist_ac**2.0)+.(dist_ab**2.0)+.(dist_bc**2.0) in
        let res2 = res1 /. (2.0 *. dist_ab *.dist_bc) in
        let res = acos res2 in
        if cmp_float res (pi -. params.epsilon) = -1 ||
           cmp_float res (pi +. params.epsilon) = 1 then
          true
        else
          loop (i+1) top
  in
  let res =
    if numpoints < 5 then false
    else loop 0 (numpoints - (params.c_pts+params.d_pts))
  in
  cmv.(9) <- res

let lic10 =
  let rec loop i top =
    if i >= top then false
    else
      let a = points.(i) in
      let b = points.(i+params.e_pts) in
      let c = points.(i+params.e_pts+params.f_pts) in
      let area = triangle_area a b c in
      if cmp_float area params.area1 = 1 then
        true
      else
        loop (i+1) top
  in
  let res =
    if numpoints < 5 then true
    else loop 0 (numpoints - (params.e_pts+params.f_pts))
  in
  cmv.(10) <- res

let lic11 =
  let rec loop i top =
    if i >= top then false
    else
      let p1 = points.(i) in
      let p2 = points.(i+params.g_pts) in
      if cmp_float p2.x p1.x = -1 then
        true
      else
        loop (i+1) top
  in
  let res =
    if numpoints < 3 then false
    else
      loop 0 (numpoints - params.g_pts)
  in
  cmv.(11) <- res

let lic12 =
  let c1 = ref false in
  let c2 = ref false in
  let rec loop i top =
    if i >= top then false
    else begin
        let p1 = points.(i) in
        let p2 = points.(i+params.k_pts) in
        if not !c1 then begin
            if cmp_float (dist p1 p2) params.length1 = 1 then
              c1 := true
          end;
        if not !c2 then begin
            if cmp_float (dist p1 p2) params.length2 = -1 then
              c2 := true
          end;
        if !c1 && !c2 then
          true
        else
          loop (i+1) top
      end
  in
  let res =
    if numpoints < 3 then false
    else loop 0 (numpoints - params.k_pts)
  in
  cmv.(12) <- res

let lic13 =
  let c1 = ref false in
  let c2 = ref false in
  let rec loop i top =
    if i >= top then false
    else begin
        let a = points.(i) in
        let b = points.(i+params.a_pts) in
        let c = points.(i+params.a_pts+params.b_pts) in
        let dist_bc = dist b c in
        let dist_ac = dist a c in
        let dist_ab = dist a b in
        let area = triangle_area a b c in
        let abc = dist_bc *. dist_ac *. dist_ab in
        let r2 = abc /. (2.0*.area) in
        let r = r2 /. 2.0 in
        if not !c1 then begin
            if cmp_float r params.radius1 = 1 then
              c1 := true
          end;
        if not !c2 then begin
            if cmp_float r params.radius2 = -1 then
              c2 := true
          end;
        if !c1 && !c2 then
          true
        else
          loop (i+1) top
      end
  in
  let res =
    if numpoints < 5 then false
    else loop 0 (numpoints - (params.a_pts+params.b_pts))
  in
  cmv.(13) <- res

let lic14 =
  let c1 = ref false in
  let c2 = ref false in
  let rec loop i top =
    if i >= top then false
    else begin
        let a = points.(i) in
        let b = points.(i+params.e_pts) in
        let c = points.(i+params.e_pts+params.f_pts) in
        let area = triangle_area a b c in
        if not !c1 then begin
            if cmp_float area params.area1 = 1 then
              c1 := true
          end;
        if not !c2 then begin
            if cmp_float area params.area2 = -1 then
              c2 := true
          end;
        if !c1 && !c2 then
          true
        else
          loop (i+1) top
      end
  in
  let res =
    if numpoints < 5 then false
    else loop 0 (numpoints - (params.e_pts+params.f_pts))
  in
  cmv.(14) <- res

let pum = Array.make_matrix 15 15 true

let create_pum =
  for i = 0 to 14 do
    for j = 0 to 14 do
      let cmv1 = cmv.(i) in
      let cmv2 = cmv.(j) in
      pum.(i).(j) <- lcm.(i).(j) cmv1 cmv2
    done
  done

let for_all a p =
  let rec loop i top =
    if i >= top then true
    else
      p a.(i) && loop (i+1) top
  in
  loop 0 (Array.length a)

let fuv = Array.make 15 true

let create_fuv =
  for i = 0 to 14 do
    let res =
      if not puv.(i) then
        true
      else
        for_all pum.(i) (fun e -> e)
    in
    fuv.(i) <- res
  done

let launch =
  for_all fuv (fun e -> e)

let () =
  Format.printf "%s@." ((fun x -> if x then "yes" else "no") launch)
