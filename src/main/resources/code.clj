(File
  "assets"
  [% (GenDecl
        import
        [% (ImportSpec
              nil
              (const "go/token")
              )
            ]
        )
      (GenDecl
        type
        [% (TypeSpec
              "Node"
              (InterfaceType
                (FieldList
                  [% (Field
                        [% "Pos"
                            ]
                        (FuncType
                          (FieldList
                            [% ]
                            )
                          (FieldList
                            [% (Field
                                  [% ]
                                  (SelectorExpr
                                    "token"
                                    "Pos"
                                    )
                                  nil
                                  )
                                ]
                            )
                          )
                        nil
                        )
                      (Field
                        [% "End"
                            ]
                        (FuncType
                          (FieldList
                            [% ]
                            )
                          (FieldList
                            [% (Field
                                  [% ]
                                  (SelectorExpr
                                    "token"
                                    "Pos"
                                    )
                                  nil
                                  )
                                ]
                            )
                          )
                        nil
                        )
                      ]
                  )
                false
                )
              )
            ]
        )
      ]
  nil
  [% (ImportSpec
        nil
        (const "go/token")
        )
      ]
  [% "token"
      "token"
      ]
  [% ]
  )