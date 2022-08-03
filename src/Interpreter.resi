let interpret: (
  ~initialEnv: Env.t<Value.t>=?,
  list<Ast.stmt>,
) => result<Env.t<Value.t>, (string, Location.t)>
