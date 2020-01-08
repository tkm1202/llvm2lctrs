open Option
let rec concatlist slist =
match slist with
 | [] -> ""
 | (s::ss) -> s ^ "\n" ^ concatlist ss 
;;

type wrap = Nuw | Nsw | NuwNsw | Nothing ;;
let string_of_wrap w = match w with
 | Nuw -> "nuw "
 | Nsw -> "nsw "
 | NuwNsw -> "nuw nsw "
 | Nothing -> ""
;;

type typ = I of int ;;
let string_of_typ (I n) = "i"^(string_of_int n)^" ";;

type expr = Int of int
  | Var of string ;;
let string_of_expr expr =
  match expr with
    | Int i -> (string_of_int i)^" "
    | Var v -> v^" " ;;

type label = Labelint of int
  | Labelvar of string ;;
let string_of_label label =
  match label with
    | Labelint i -> (string_of_int i)^" "
    | Labelvar v -> v^" " ;;

let string_of_arg (t,e) = (string_of_typ t) ^ (string_of_expr e)
;;

type phi_node = Phinode of expr * label ;;
let string_of_phi_node (Phinode (e,l)) = "[ " ^ (string_of_expr e) ^ (string_of_label l) ^ "] "
;;
type phi_instruction = Phi of typ * phi_node list ;;
let rec string_of_phi (Phi (t,ps))=  "phi " ^ (string_of_typ t) ^ (concatlist (List.map string_of_phi_node ps))
;;

type switchbr = typ * string * label ;;
let string_of_switchbr (t,s,l) = (string_of_typ t) ^ s ^ " " ^ (string_of_label l)
;;

type terminate_instruction = Return of (typ * expr) option
  | Branch of typ * string * label * label
  | Goto of label (* br label l *)
  | Switch of typ * string * label * switchbr list ;;
let string_of_terminate_instruction terminate_instruction =
match terminate_instruction with
    | Return typexpr -> if is_some typexpr then let (t,e) = get typexpr in "ret " ^ (string_of_typ t) ^ (string_of_expr e) else "ret void"
    | Branch (t,s,l1,l2) -> "br " ^ (string_of_typ t) ^ s ^ " " ^ (string_of_label l1) ^ (string_of_label l2)
    | Goto l -> "br " ^ (string_of_label l)
    | Switch (t,s,l,ss) -> "switch " ^ (string_of_typ t) ^ s ^ (string_of_label l) ^ (concatlist (List.map string_of_switchbr ss))
;;

type icond = Eq | Ne | Ugt | Uge | Ult | Ule | Sgt | Sge | Slt | Sle ;;
let string_of_icond icond =
  match icond with
  | Eq -> "eq "
  | Ne -> "ne "
  | Ugt -> "ugt "
  | Uge -> "uge "
  | Ult -> "ult "
  | Ule -> "ule "
  | Sgt -> "sgt "
  | Sge -> "sge "
  | Slt -> "slt "
  | Sle -> "sle "
;;

type fcond = False | Oeq | Ogt | Oge | Olt | Ole | One | Ord | Ueq | Ugt | Uge | Ult | Ule | Une | Uno | True ;;
let string_of_fcond fcond =
  match fcond with
  | False -> "false "
  | Oeq -> "oeq "
  | Ogt -> "ogt "
  | Oge -> "oge "
  | Olt -> "olt "
  | Ole -> "ole "
  | One -> "one "
  | Ord -> "ord "
  | Ueq -> "ueq "
  | Ugt -> "ugt "
  | Uge -> "uge "
  | Ult -> "ult "
  | Ule -> "ule "
  | Une -> "une "
  | Uno -> "uno "
  | True -> "true "
;;

type instruction =
(* unary_instruction *) 
  | Fneg of string * typ * expr
(* binary_instruction *) 
  | Add of string * wrap * typ * expr * expr
  | Fadd of string * typ * expr * expr
  | Sub of string * wrap * typ * expr * expr
  | Fsub of string * typ * expr * expr
  | Mul of string * wrap * typ * expr * expr
  | Fmul of string * typ * expr * expr
  | Udiv of string * bool * typ * expr * expr
  | Sdiv of string * bool * typ * expr * expr
  | Fdiv of string * typ * expr * expr
  | Urem of string * typ * expr * expr
  | Srem of string * typ * expr * expr
  | Frem of string * typ * expr * expr
(*  bitwise_instruction *)
  | Shl of string * wrap * typ * expr * expr
  | Lshr of string * bool * typ * expr * expr
  | Ashr of string * bool * typ * expr * expr
  | And of string * typ * expr * expr
  | Or of string * typ * expr * expr
  | Xor of string * typ * expr * expr
(* memory_instruction *)
  | Alloca of string * typ * (typ * int) option * int option
  | Load of string * typ * typ * string * int option
  | Store of typ * expr * typ * string * int option
(* conv_instruction *)
  | Trunc of string * typ * string * typ
  | Zext of string * typ * string * typ
  | Sext of string * typ * string * typ
  | Fptrunc of string * typ * string * typ
  | Fpext of string * typ * string * typ
  | Fptoui of string * typ * string * typ
  | Fptosi of string * typ * string * typ
  | Uitofp of string * typ * string * typ
  | Sitofp of string * typ * string * typ
(* other_instruction *)
  | Icmp of string * icond * typ * expr * expr
  | Fcmp of string * fcond * typ * expr * expr
  | Call of string * typ * string * (typ * expr) list
;;

let string_of_instruction i =
match i with
  | Fneg (s,t,e) -> s ^  " = fneg " ^ (string_of_typ t) ^ (string_of_expr e)
  | Add (s,w,t,e1,e2) -> s ^ " = add " ^ (string_of_wrap w) ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Fadd (s,t,e1,e2) -> s ^ " = fadd " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Sub (s,w,t,e1,e2) -> s ^ " = sub " ^ (string_of_wrap w) ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Fsub (s,t,e1,e2) -> s ^ " = fsub " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Mul (s,w,t,e1,e2) -> s ^ " = mul " ^ (string_of_wrap w)  ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Fmul (s,t,e1,e2) -> s ^ " = fmul " ^ (string_of_typ t)  ^ (string_of_expr e1)  ^ (string_of_expr e2)
  | Udiv (s,b,t,e1,e2) -> if b then s ^ " = udiv exact " ^ (string_of_typ t)  ^ (string_of_expr e1)  ^ (string_of_expr e2)
else s ^ " = udiv " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Sdiv (s,b,t,e1,e2) -> if b then s ^ " = sdiv exact " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
else s ^ " = sdiv " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Fdiv (s,t,e1,e2) -> s ^ " = fdiv " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Urem (s,t,e1,e2) -> s ^ " = urem " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Srem (s,t,e1,e2) -> s ^ " = srem " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Frem (s,t,e1,e2) -> s ^ " = frem " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Shl (s,w,t,e1,e2) -> s ^ " = shl " ^ (string_of_wrap w) ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Lshr (s,b,t,e1,e2) -> if b then s ^ " = lshr exact " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2) else s ^ " = lshr " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Ashr (s,b,t,e1,e2) -> if b then s ^ " = ashr exact " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2) else s ^ " = ashr " ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | And (s,t,e1,e2) -> s ^ " = and " ^  (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Or (s,t,e1,e2) -> s ^ " = or " ^  (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Xor (s,t,e1,e2) -> s ^ " = xor " ^  (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Alloca (s,t1,typint,n2) -> if is_some typint then let (t2,n1) = get typint in (if is_some n2 then s ^ " = alloca " ^  (string_of_typ t1) ^ (string_of_typ t2) ^ (string_of_int n1) ^ (string_of_int (get n2)) else s ^ " = alloca " ^  (string_of_typ t1) ^ (string_of_typ t2) ^ (string_of_int n1)) else (if is_some n2 then s ^ " = alloca " ^  (string_of_typ t1) ^ (string_of_int (get n2)) else s ^ " = alloca " ^  (string_of_typ t1))
  | Load (s1,t1,t2,s2,n) -> if is_some n then s1 ^ " = load " ^  (string_of_typ t1) ^ (string_of_typ t2) ^ s2 ^ (string_of_int (get n)) else s1 ^ " = load " ^  (string_of_typ t1) ^ (string_of_typ t2) ^ s2
  | Store (t1,e,t2,s,n) -> if is_some n then "store " ^  (string_of_typ t1) ^ (string_of_expr e) ^ (string_of_typ t2) ^ s ^ (string_of_int (get n)) else "store " ^  (string_of_typ t1) ^ (string_of_expr e) ^ (string_of_typ t2) ^ s
  | Trunc (s1,t1,s2,t2) -> s1 ^ " = trunc " ^ (string_of_typ t1) ^ s2 ^ "to " ^ (string_of_typ t2)
  | Zext (s1,t1,s2,t2) -> s1 ^ " = zext " ^ (string_of_typ t1) ^ s2 ^ "to " ^ (string_of_typ t2)
  | Sext (s1,t1,s2,t2) -> s1 ^ " = sext " ^ (string_of_typ t1) ^ s2 ^ "to " ^ (string_of_typ t2)
  | Fptrunc (s1,t1,s2,t2) -> s1 ^ " = fptrunc " ^ (string_of_typ t1) ^ s2 ^ "to " ^ (string_of_typ t2)
  | Fpext (s1,t1,s2,t2) -> s1 ^ " = fpext " ^ (string_of_typ t1) ^ s2 ^ "to " ^ (string_of_typ t2)
  | Fptoui (s1,t1,s2,t2) -> s1 ^ " = fptoui " ^ (string_of_typ t1) ^ s2 ^ "to " ^ (string_of_typ t2)
  | Fptosi (s1,t1,s2,t2) -> s1 ^ " = fptosi " ^ (string_of_typ t1) ^ s2 ^ "to " ^ (string_of_typ t2)
  | Uitofp (s1,t1,s2,t2) -> s1 ^ " = uitofp " ^ (string_of_typ t1) ^ s2 ^ "to " ^ (string_of_typ t2)
  | Sitofp (s1,t1,s2,t2) -> s1 ^ " = sitofp " ^ (string_of_typ t1) ^ s2 ^ "to " ^ (string_of_typ t2)
  | Icmp (s,icond,t,e1,e2) -> s ^ " = icmp " ^ (string_of_icond icond) ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Fcmp (s,fcond,t,e1,e2) -> s ^ " = fcmp " ^ (string_of_fcond fcond) ^ (string_of_typ t) ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Call (s1,t,s2,args) -> s1 ^ " = call " ^ (string_of_typ t) ^ s2 ^ (concatlist (List.map string_of_arg args))
;;

type block = Block of phi_instruction list * instruction list * terminate_instruction ;;
let string_of_block ((Block (ps,is,ti)):block) = (concatlist (List.map string_of_phi ps)) ^ (concatlist (List.map string_of_instruction is)) ^ (string_of_terminate_instruction ti)
;;

type fundecl = typ * string * (typ list) * (block list) ;;
let string_of_fundecl ((t,s,types,bs) : fundecl) = (string_of_typ t) ^ s ^ (concatlist (List.map string_of_typ types)) ^ (concatlist (List.map string_of_block bs))
;;
