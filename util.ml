let rand (bytes: int) = 
    let urandom = open_in "/dev/urandom" in
    let ret = Bytes.create bytes in
    ignore (input urandom ret 0 bytes);
    ret
;;

let rand_z (p: Z.t) = 
    let rb = rand ((Z.log2 p)/8) in
    let hex = Hex.show (Hex.of_string (Bytes.to_string rb)) in
    let r1 = Z.of_string_base 16 hex in
    let r2 = Z.rem r1 p in
    match r2 with
        | r2 when (Z.equal r2 Z.zero) -> Z.one
        | _ -> r2
;;
