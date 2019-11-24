let rand (bytes: int) = 
    let urandom = open_in "/dev/urandom" in
    let ret = Bytes.create bytes in
    ignore (input urandom ret 0 bytes);
    ret
;;

let rec rand_z (p: Z.t) = 
    let rb = rand ((Z.log2 p)/8) in
    let hex = Hex.show (Hex.of_string (Bytes.to_string rb)) in
    let r1 = Z.of_string_base 16 hex in
    match r1 with
        | r1 when (Z.equal r1 Z.zero) -> Z.one
        | r1 when (Z.gt r1 p) ->  rand_z p
        | _ -> r1
;;
