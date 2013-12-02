newvar = newTypeVar "a"
addToEnv name t = \(TypeEnv env) -> TypeEnv $ M.insert name t env

(|-) = teUnion
infixr 2 |-
(==>) = singleEnv
infixr 5 ==>

infer :: Expr -> Inferrer Type
infer expr = case expr of
  String _ -> return StringType
  Number _ -> return NumberType
  Symbol s -> infer $ Var s
  Var name -> do
    polys <- namesToPolytypes <$> get
    case teLookup name polys of
      Nothing -> throwError $ "unbound variable: " ++ name
      Just σ -> instantiate σ
  Apply func arg -> do
    newT  <- newvar
    argT  <- infer arg
    funcT <- infer func
    resS  <- unify funcT (argT :=> newT)
    return newT
  Lambda param body -> do
    paramT <- newvar
    updateEnv $ (|- x ==> Polytype [] paramT) . remove x
    bodyT <- infer body
    return $ paramT :=> bodyT
  Let var body next -> do
    bodyT <- infer body
    generalize bodyT x -- should remove x from env and then assign it to the generalized version
    case next of
      Nothing -> return $ TupleType []
      Just expr -> infer expr
