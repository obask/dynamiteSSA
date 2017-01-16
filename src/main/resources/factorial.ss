(DefDef
      fact
      Nil
      [% [% (ValDef n (t Int) EmptyTree)]]
      (t Int)
      (Block
       [%
            (ValDef n1 (t Int) (Apply (Select (Ident n) $minus) [% (Literal (Constant 1))]))
       ]
       (Typed
               (If
                (Apply (Select (Ident n) $less) [% (Literal (Constant 2))])
                (Literal (Constant 1))
                (Apply (Select (Ident n) $times) [% (Apply (Ident fact) [% (Ident n1)])]))
               (tn Int))))
