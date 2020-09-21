let somme_v3 a b f =
	let rec boucle i accu =
		if i<= b then
			boucle (i+1)(accu + f i)
		else
			accu
	in
	boucle(0);;



(*-------TD1-------*)

let max2 a b =
  if a >= b then a else b
;;
let max3 a b c = max2 a (max2 b c)
;;



let divisiblePar a b =
  if b = 0 then
    a=0
  else
    a mod b = 0;;
let ex_10par5 = divisiblePar 10 5;;
divisiblePar 15 3;;
divisiblePar 10 5;;



(* TYPE ALGEBRIQUE *)
type hms = HMS of int * int * int;;
HMS (23, 12, 15)
;;
let heureSuivante x =
  match x with
  | HMS (h, m, s) -> HMS (h+1, m, s);;
let heureSuivante (HMS (h, m, s)) = HMS (h+1, m, s);;

Type frac =
  | Frac of int * int
  | NaN(* pas un nombre*);;

(*
let testEgalite (Frac(a, b)) (Frac (a', b')) =
  a * b' = a' + b;;
 *)


let testEgalite x y =
  match(x, y) with
  | (Frac (a, b), Frac(a', b')) -> a * b' = b * a'
  | (NaN, NaN) -> true
  | _ -> false;;


type etu = Etu of string * int;; (*nom, note*)

let meilleur (Etu (_, notea)) (Etu(_, noteb))=
  (notea >= noteb)
;;

let meilleur x y =
  match (x, y) with
    ((Etu (_, notea)) (Etu (_, noteb))) -> (notea >= noteb)
;;

let classement a b c =
  let (a', b') =
    if meilleur a b then (a, b) else (b, a) in
  c' = c in
    if meilleur c' a' then
      (c', a', b')
    else if meilleur b' c' then
      (a', b', c')
    else
      (a' , c' ,b')
;;


type date = Date of int * int * int (*J M A*)

let jourDansMois m a
      if m = 2 then
        if bissextile a then 29 else 28
      
