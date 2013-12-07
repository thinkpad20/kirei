unifying (TypeVar "c" :=> TypeVar "c",
  (TypeVar "d" :=> TypeVar "d") :=> TypeVar "a") with Env {bound = {}, rs = {"a" => c -> c, "b" => c -> c}, free = {"id" => b -> b}, names = fromList ["a","b","c","d"]}

unify :: Env -> (Type, Type) -> Env
unify env (t1, t2) = u env (refine t1 env, refine t2 env) where
  u env (TypeVar v, t) = env {rs = tmInsert v t (env.rs) }
  u env (t, TypeVar v) = env {rs = tmInsert v t (env.rs) }
  u env (a :=> b, c :=> d) = trace ("u function 3) env'' = " ++ show env'') env'' where
    env'  = trace ("u function 1) unifying " ++ show (a, c) ++ " with " ++ show env) (unify env (a, c))
    env'' = trace ("u function 2) unifying " ++ show (b, d) ++ " with " ++ show env') (unify env' (b, d))
  u env (NamedType n ts, NamedType n' ts') | n == n' =
    foldr envUnion envEmpty $ unify env <$> (zip ts ts')
  u _ (t1, t2) = error $ "Types don't unify: " ++ show t1 ++ " !: " ++ show t2

{-
ok rough...

let id =
  \x ->
    let y = x;
    y;
so this suggests something unfortunate... that we can't always determine the type by the end of the let statement... :(
But clearly this is because the thing depends on something which is part of the bound variables...

initialize {x: a}
then say let y = x so {y: a}, but 'a' is still a type variable in the free scope! So what does this mean?

it means when we bring in y, we would instantiate it, but its type is in the free scope... hmmmm

# these should all be type a -> a
let id = \x -> x;
# now id : a -> a; bound = {}, rs = {}, free = {id: a-> a}
let id1 =
  bound = {id1: a}, rs = {}, free = {id: a->a}
  \x ->
    bound = {id1: a, x: b}, rs = {}, free = {id: a->a}
    let y =
      bound = {id1: a, x: b, y: c}, rs = {}, free = {id: a->a}
      x;
    bound = {id1: a, x: b}, rs = {}, free = {id: a->a, y: b}
    y;

let id2 = \x -> let y = id x; id1 id y;

krrp... HM might be best after all

-----------------

what if instead of bound vs free vars, we have bound vs free type vars?
- set of bound type vars
- set of type constraints
- mapping from vars to types

let inc = \x -> x+1; inc;
{}, {}, {+:N->N->N}
let inc =
  {a}, {}, {inc:a}
  \x ->
    {a, b}, {}, {inc:a, x:b, +:N->N->N}
    infer ((Apply (+ x) 1))
      infer (Apply + x)
        infer +
          lookup + -> N->N->N
        infer x
          lookup x -> b
        newvar -> c
        unify (funcT, argT :=> newvar)
          (N->N->N, b -> c)
          {a, b, c}, {b: N, c: N->N}, {inc: a, x: b, +: N->N->N}
        return refine c -> (N->N) <- can we get rid of c now?
      infer 1
        return N
      newvar -> d
      unify (funcT, argT :=> newvar)
        (N->N, N -> d)
        {a,b,c,d}, {b:N, c:N->N}, {inc:a, x:b, +:N->N->N}
      return refine d -> N
    paramT = refine x -> N
    return (paramT :=> exprT) = (N -> N) <- now we can get rid of x


let id = \x -> let y = x; y; id;
infer (Let id (Lambda x (Let y x (y)))) id ({}, {}, {})
  infer (Lambda x (Let y x y)) ({a}, {}, {id:a})
    infer (Let y x y) ({a, b} {} {id:a, x:b})
      resultT = infer x ({a, b, c}, {}, {id:a, x:b, y:c})
        lookup x -> b
      unify (type of y, resultT)
        unify (c, b)
          return ({a, b, c}, {c:b}, {id:a, x:b, y:c})
      return resulT (b)
      infer y
        lookup y -> c
        return refine c -> b
    paramT = refine type of x -> b
    return (paramT :=> resultT) -> (b -> b)
  unify (a, b-> b)
    {a, b, c}, {a: b -> b, c: b}, {id: a}
  return (b -> b)


-}
