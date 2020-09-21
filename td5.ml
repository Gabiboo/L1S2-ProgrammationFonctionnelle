(***DAGORNE Gabriel TD5***)

(* Exo 1 *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree
;;

let rec taille arbre=
match arbre with
| Nil -> 0
|Node (x , a , b) -> 1 + taille a + taille b
;;

let rec hauteur arbre =
match arbre with
| Nil -> 0
|Node (x , a , b) -> 1 +max (hauteur a) (hauteur b)
;;

let exemple =
Node(17 , Node(3 , Nil , Nil), Node(48,Nil,Nil))
;;
let t = taille exemple;;
let h = hauteur exemple;;

(* facile mais inéfficace *)
let rec equitaille arbre =
match arbre with
| Nil -> true
|Node (x , a , b) -> 
let ta = taille a in
let tb = taille b in
equitaille a && equitaille b &&
( -1 <= ta - tb) && (ta - tb <= 1)
;;

(* beaucoup mieux: équilibré ==> Some taille, Sinon ==> None *)
let recf equitaille' arbre =
match arbre with
| Nil -> Some 0
| Node (x, a, b) -> match (equitaille' a, equitaille' b) with
| (Some ta, Some tb) -> if ( -1 <= ta - tb) && (ta - tb <= 1) then
Some ( 1+ ta +tb)
else 
None
| _ -> None
;;

(*let equitaille arbre =
match equitaille' arbre with
| Some _ -> true
| None -> false
;;*)

(* infixe : liste des noeuds de GAUCHE à DROITE *)
(* facile && inefficace *)

let rec infixe arbre =
match arbre with
|Nil -> []
|Node (x , a , b) -> infixe a @ [x] @ infixe b
;;

let f x = Node (x, Nil, Nil);; (* f pour feuille *)

let u = Node ("U" , f "V" , f "w");;
let a = Node("A" , f" C" , u);;
let z = Node ("Z" , Nil , f "T");;
let b = Node ("B" , f "Y" , z);;
let x = Node ("X", a , b);;

let liste = infixe x ;; 




(* efficacité : rajout d'une suite *)
(* infixe arbre suite calcule (infixe arbre ) @n suite *)

let rec infixe' arbre suite=
match arbre with
| Nil -> suite
|Node (x , a , b) -> 
let bsuite = infixe' b suite in 
let xbsuite = x :: bsuite in
let axbsuite = infixe' a xbsuite in
axbsuite
;;

let rec infixe' arbre suite=
match arbre with
| Nil -> suite
|Node (x , a , b) -> infixe' a (x :: (infixe' b suite))

;;

let infixe arbre = infixe' arbre []
;; 

let liste = infixe x;;

(* CONTRAIRE AU SUJET : string tree au lieu de tree *)

let rec prefixe arbre =
match arbre with
| Nil -> "Nil"
|Node (x , a , b) -> "Node (\""^ x ^"\"," ^ prefixe a ^ "," ^ prefixe b ^ ")"
;;

print_endline (prefixe u);;
print_endline (prefixe x);;

(*let x1 = x;;
(* let x2 = COPIER-COLLER;; *)
let x2 = 
let test = (x1 = x2);;*)

let rec droite liste =
match liste with 
| [] -> Nil
|h::t -> Node (h , Nil , droite t)
;;

let rec gauche' liste = 
match liste with
| [] -> Nil
| h :: t -> Node ( h , gauche' t , Nil)
;;

let gauche liste = gauche' (List.rev liste)
;;
let liste0 = [1; 3; 8; 37; 56];;
droite liste;;
infixe (droite liste);;
gauche liste;;
infixe (gauche liste);;


(** exo 3 **)
(** q2 **)

let rec find_opt clef table =
match table with
| Nil -> None
|Node ((c,v) , a , b) -> if clef < c then 
find_opt clef a
else if clef > c then
find_opt clef b
else (* forcément clef = c*)
Some v
;;


let rec add clef valeur table =
match table with
| Nil -> Node ((clef, valeur ), Nil, Nil)
| Node ((c,v) , a , b) -> if clef < c then 
Node ((c,v), add clef valeur a, b)
else if clef > c then
Node ((c,v), a, add clef valeur b)
else (* forcément clef = c*)
Node ((clef, valeur), a, b);;