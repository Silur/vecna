let p = Z.of_string "1018685373207772240070628198584242295057596711413469431050783788716179587403678060756398020221016483104802870361253655689004710993786562063651966799397114446698383666236977978598610573997193363730553820504093654773789787633431569965647789474696067006211223045576798453828459888097946937632078627754878749022535366024832558956660685776044620735107862337314735247095551385969882483538453002120284740618363611988479188612465677344665266586565739614822121753545594924508051569280254669900432189343707020702784819847545546905345508547907845058007070023705296050172201535647172961154931459457396731138635006019070021867185195150640154941394377499553553659919103777547855317098341140982612584073710013865187696573195001788358783960051889563082575367530015304198638176062052774674114690587370131243466918803523187273435541218934827478143151020774678428163328341874763913110003003724710069554464317725639312149635795919358541581307194818657147158983581501761275097698585337744623074083863610221930572374632312714594117092967061598239112225536494679923613775026668085296186694747053052060379423007242634862966075515227594926804370803014080422961200910863300935282499463830670990094715278237196225531469156916867258161520369960262136804361817729";;

let log2 n = 
    log n /. log 2.0
;;

let fsub a b =
    let x = Z.sub a b in
    if Z.lt x Z.zero then Z.add x p
    else x
;;

let rec gcd_ext (a: Z.t) (b: Z.t) =
    if (Z.equal b Z.zero) then (Z.one, Z.zero, a)
    else
      let s, t, g = gcd_ext b (Z.rem a b) in
      (t, (fsub s (Z.mul ((Z.div a b)) t)), g)
;;

let mod_inv a m =
  let mk_pos x = if (Z.lt x Z.zero) then Z.add x m else x in
  let (i, _, check) = gcd_ext a m in
   if (Z.equal (Z.abs check) Z.one) then mk_pos i
   else failwith "mod_inv"
;;

let horner coeffs (x: Z.t) =
    Z.rem (List.fold_left (fun acc coef -> Z.add (Z.mul acc x) coef) Z.zero (List.rev coeffs)) p
;;

let lagrange x xs ys = 
	let k = List.length xs in
	let l v = List.fold_left (fun acc value -> Z.mul acc value) Z.one v in
	let nums = Array.make k Z.zero in
	let dens = Array.make k Z.zero in
	List.iteri (fun i v -> begin
		let cur = List.find (fun v2 -> Z.equal v v2) xs in
		let others = List.filter (fun v2 -> not (Z.equal v v2)) xs in
        Array.set nums i (l (List.map (fun o -> fsub x o) others));
        Array.set dens i (l (List.map (fun o -> fsub cur o) others));
    end) xs;
	let den = l (Array.to_list dens) in
	let num = Array.make k Z.one in
	for i=0 to (k-1) do
        let densi = Array.get dens i in
        let numsi = Array.get nums i in
        let ysi = List.nth ys i in
		Array.set num i (Z.mul (Z.rem (Z.mul (Z.mul numsi den) ysi) p)(mod_inv densi p))
    done;
    let num = Array.to_list num in
    let num = List.fold_left (fun acc v -> Z.add acc v) Z.zero num in
    Z.rem (Z.add (Z.mul num (mod_inv den p)) p) p
;;

let print_point (i, p) =
    let istr = string_of_int (Z.to_int i) in
    let pstr = Z.to_string p in
    print_endline (String.concat " " [istr; pstr])

let split secret n k =
    if (Z.log2 secret) > 4096 then
        invalid_arg "secret too long"
    else
        let coefs = [secret]@(List.init (k-1) (fun i-> Util.rand_z p)) in
        let points = List.init n (fun i -> ((Z.of_int (i+1)), horner coefs (Z.of_int (i+1)))) in
        points
;;

let combine secrets = 
    let xs = List.map ( fun v -> 
        let (x,_) = v in
        x) secrets in
    let ys = List.map (fun v -> 
        let (_,y) = v in
        y) secrets in
    lagrange Z.zero xs ys
;;

let secrets = split (Z.of_string "11674631618809742415950651977929399199110281870698673792627881586330283847844443182092892822248352486583821897208004407519682957687232586310466102913944443" ) 6 3 in
List.iter (fun i -> print_point i) secrets;
let solution = combine (List.tl (List.tl (List.tl secrets))) in
print_endline (Z.to_string solution)
