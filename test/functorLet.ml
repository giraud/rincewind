module type X_int = sig val x : int end

module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end

(*
4.06

[]
[]
[
  structure_item (./test/functorLet.ml[1,0+0].../test/functorLet.ml[1,0+39])
    Tstr_modtype "X_int/1010"
      module_type (./test/functorLet.ml[1,0+20].../test/functorLet.ml[1,0+39])
        Tmty_signature
        [
          signature_item (./test/functorLet.ml[1,0+24].../test/functorLet.ml[1,0+35])
            Tsig_value
            value_description x/1009 (./test/functorLet.ml[1,0+24].../test/functorLet.ml[1,0+35])
              core_type (./test/functorLet.ml[1,0+32].../test/functorLet.ml[1,0+35])
                Ttyp_constr "int/1"
                []
              []
        ]
]
[]
[
  structure_item (./test/functorLet.ml[3,41+0].../test/functorLet.ml[5,105+3])
    Tstr_module
    Increment/1012
      module_expr (./test/functorLet.ml[3,41+17].../test/functorLet.ml[5,105+3])
        Tmod_functor "M/1013"
        module_type (./test/functorLet.ml[3,41+22].../test/functorLet.ml[3,41+27])
          Tmty_ident "X_int/1010"
        module_expr (./test/functorLet.ml[3,41+29].../test/functorLet.ml[5,105+3])
          Tmod_constraint
          module_expr (./test/functorLet.ml[3,41+39].../test/functorLet.ml[5,105+3])
            Tmod_structure
            [
              structure_item (./test/functorLet.ml[4,87+2].../test/functorLet.ml[4,87+17])
                Tstr_value Nonrec
                [
                  <def>
                    pattern (./test/functorLet.ml[4,87+6].../test/functorLet.ml[4,87+7])
                      Tpat_var "x/1014"
                    expression (./test/functorLet.ml[4,87+10].../test/functorLet.ml[4,87+17])
                      Texp_apply
                      expression (./test/functorLet.ml[4,87+14].../test/functorLet.ml[4,87+15])
                        Texp_ident "Pervasives!.+"
                      [
                        <arg>
                          Nolabel
                          expression (./test/functorLet.ml[4,87+10].../test/functorLet.ml[4,87+13])
                            Texp_ident "M/1013.x"
                        <arg>
                          Nolabel
                          expression (./test/functorLet.ml[4,87+16].../test/functorLet.ml[4,87+17])
                            Texp_constant Const_int 1
                      ]
                ]
            ]
          module_type (./test/functorLet.ml[3,41+31].../test/functorLet.ml[3,41+36])
            Tmty_ident "X_int/1010"
]
[]
*)