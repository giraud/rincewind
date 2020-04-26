module type X_int = sig val x : int end

module Make (M : X_int) : X_int = struct
  let x = M.x + 1
end

module Impl = Make(struct
    let x = 3
end)