let rec lagrange1 lst x xj = match lst with [] -> 1.0
|(xn, yn)::tail -> if xn =xj then lagrange1 tail x xj else ((x -. xn) /. (xj -. xn)) *. lagrange1 tail x xj;;
  

let rec pi_sum l lst x =match lst with [] -> 0.0
|(xn, yn)::tail -> (yn *. lagrange1 l x xn) +.pi_sum l tail x;;

let lagrange lst x = 
  pi_sum lst lst x;;
