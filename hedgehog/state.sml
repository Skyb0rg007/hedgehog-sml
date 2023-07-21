
signature STATE =
  sig
    structure Var :
      sig
        type 'a t
        type id

        (* This is used for implementing `vars` - see `Command.new` *)
        val id : 'a t -> id

        (* This should only be used for debugging *)
        val toString : 'a t -> string
      end

    structure Env :
      sig
        type t

        val lookup : t * 'a Var.t -> 'a
      end

    structure Callback :
      sig
        type ('input, 'output, 'state) t

        (* Precondition before applying input to the state *)
        val require : ('state * 'input -> bool) -> ('input, 'output, 'state) t

        (* Update the model state
         * The output variable is not necessarily bound, so the only thing
         * you can do with it is save it into the updated state *)
        val update : ('state * 'input * 'output Var.t -> 'state) -> ('input, 'output, 'state) t

        (* Postcondition for a command
         * You have access to an environment since the variables are all bound
         * This is where the model testing does its work *)
        (* XXX: should be in Test monad *)
        val ensure : ('state * 'state * 'input * 'output * Env.t -> bool) -> ('input, 'output, 'state) t
      end

    structure Command :
      sig
        type 'state t

        val new :
           (* Given the model state, generate a possible input.
            * Can return NONE if there are no valid inputs, or if you are
            * testing a command that requires a different state *)
          {generate : 'state -> 'input Gen.t option,
           (* Returns a list of all the embedded variables in an input
            * This is needed for proper shrinking - get this right!
            *)
           vars : 'input -> Var.id list,
           (* Run the actual command
            * The provided environment can be used to look up variables' values
            * if they are embedded in the input type
            * Note: The environment should not be stored, it is only usable
            * in the scope of this call
            *)
           execute : 'input * Env.t -> 'output,
           (* Preconditions, Postconditions, and model updates
            * See `Callback` structure for documentation
            *)
           callbacks : ('input, 'output, 'state) Callback.t list,
           (* How to display inputs to the user *)
           inputToString : 'input -> string,
           (* How to display outputs to the user *)
           outputToString : 'output -> string}
          -> 'state t
      end

  end

structure State : STATE =
struct

  structure Var =
    struct
      datatype 'a symbolic = Sym of int * (exn -> 'a) * ('a -> exn)
      datatype 'a t = Concrete of 'a | Symbolic of 'a symbolic
      datatype id = C | S of int

      val concrete = Concrete
      val symbolic = Symbolic
      fun id (Concrete _) = C
        | id (Symbolic (Sym (n, _, _))) = S n

      val counter = ref 0

      fun 'a newSymbolic () =
        let
          exception E of 'a

          val id = !counter
          fun from (E x) = x
            | from _ = raise Fail "impossible"
        in
          counter := !counter + 1;
          Sym (id, from, E)
        end

      fun symbolicToString (Sym (n, _, _)) = "Var " ^ Int.toString n

      fun toString (Concrete _) = "<<concrete variable>>"
        | toString (Symbolic x) = symbolicToString x

      fun map f _ (Concrete x) = Concrete (f x)
        | map f g (Symbolic (Sym (n, from, to))) = Symbolic (Sym (n, f o from, to o g))
    end

  structure Scope =
    struct
      datatype t = Scope of int list

      fun inScope (Scope ids, Var.C) = true
        | inScope (Scope ids, Var.S id) = List.exists (fn n => n = id) ids

      fun allInScope (scope, ids) =
        List.all (fn id => inScope (scope, id)) ids

      val empty = Scope []

      fun insert (Scope ids, Var.Sym (n, _, _)) = Scope (n :: ids)
    end

  structure Env =
    struct
      datatype t = Env of (int * exn) list

      val empty = Env []

      fun insert (Env e, Var.Sym (id, _, to), value) =
        let
          fun loop [] = [(id, to value)]
            | loop ((id', v') :: rest) =
              case Int.compare (id, id') of
                  LESS => (id, to value) :: (id', v') :: rest
                | EQUAL => (id, to value) :: rest
                | GREATER => (id', v') :: loop rest
        in
          Env (loop e)
        end

      fun find (Env e, Var.Sym (id, from, _)) =
        let
          fun loop [] = NONE
            | loop ((id', v) :: rest) =
              case Int.compare (id, id') of
                  LESS => NONE
                | EQUAL => SOME (from v)
                | GREATER => loop rest
        in
          loop e
        end

      fun lookup (_, Var.Concrete x) = x
        | lookup (e, Var.Symbolic x) =
          case find (e, x) of
              NONE => raise Fail "Env.lookup: unbound variable"
            | SOME y => y
    end

  structure Callback =
    struct
      datatype ('input, 'output, 'state) t
        = Require of ('state * 'input -> bool)
        | Update of ('state * 'input * 'output Var.t -> 'state)
        | Ensure of ('state * 'state * 'input * 'output * Env.t -> bool)

      val require = Require
      val update = Update
      val ensure = Ensure

      fun hideTypes toI toO fromO =
        fn Require k => Require (fn (s, i) => k (s, toI i))
         | Update k => Update (fn (s, i, x) => k (s, toI i, Var.map toO fromO x))
         | Ensure k => Ensure (fn (s, s', i, x, e) => k (s, s', toI i, toO x, e))

      fun requires callbacks (state, input) =
        List.all
          (fn Require k => k (state, input)
            | _ => true)
          callbacks

      fun updates callbacks (state, input, output) =
        List.foldl
          (fn (Update k, s) => k (s, input, output)
            | (_, s) => s)
          state
          callbacks

      fun ensures callbacks (state, state', input, output, env) =
        List.all
          (fn Ensure k => k (state, state', input, output, env)
            | _ => true)
          callbacks
    end

  structure Command =
    struct
      datatype 'state t = Command of {
          gen : 'state -> exn Gen.t option,
          vars : exn -> Var.id list,
          exec : exn * Env.t -> exn,
          cbs : (exn, exn, 'state) Callback.t list,
          itos : exn -> string,
          otos : exn -> string
        }

      fun new {generate, vars, execute, callbacks, inputToString, outputToString} =
        let
          exception I of 'input
          exception O of 'output

          fun toI (I i) = i
            | toI _ = raise Fail "impossible"
          fun toO (O x) = x
            | toO _ = raise Fail "impossible"
        in
          Command {
            gen = fn s => Option.map (Gen.map I) (generate s),
            vars = vars o toI,
            exec = fn (i, e) => O (execute (toI i, e)),
            cbs = List.map (Callback.hideTypes toI toO O) callbacks,
            itos = inputToString o toI,
            otos = outputToString o toO
          }
        end

      datatype ('input, 'output, 'state) action = Action of {
          input : 'input,
          vars : Var.id list,
          soutput : 'output Var.symbolic,
          execute : 'input * Env.t -> 'output,
          require : 'state * 'input -> bool,
          update : 'state * 'input * 'output Var.t -> 'state,
          ensure : 'state * 'state * 'input * 'output * Env.t -> bool,
          itos : 'input -> string,
          otos : 'output -> string
        }

      fun showAction (Action {input, soutput, itos, ...}) =
        let
          val prefix0 = Var.symbolicToString soutput ^ " = "
          val prefix =
            CharVector.tabulate (String.size prefix0, fn _ => #" ")
          val lines = String.fields (fn c => c = #"\n") (itos input)
        in
          case lines of
              [] => prefix0 ^ "?"
            | x :: xs => String.concatWith "\n"
                (prefix0 ^ x :: List.map (fn s => prefix ^ s) xs)
        end

      fun showActions actions =
        String.concatWith "\n" (List.map showAction actions)

      fun showActionResult (Action {soutput, otos, ...}, env) =
        let
          val prefix0 = Var.symbolicToString soutput ^ " = "
          val prefix = 
            CharVector.tabulate (String.size prefix0, fn _ => #" ")
          val lines =
            case Env.find (env, soutput) of
                NONE => ["<<not found in environment>>"]
              | SOME x => String.fields (fn c => c = #"\n") (otos x)
        in
          case lines of
              [] => prefix0 ^ "?"
            | x :: xs => String.concatWith "\n"
                (prefix0 ^ x :: List.map (fn s => prefix ^ s) xs)
        end

      fun executeUpdateEnsure (action, (state, env)) =
        let
          val Action {input, soutput, execute, update, ensure, ...} = action
          val output = execute (input, env)
          val state' = update (state, input, Var.concrete output)
          val env' = Env.insert (env, soutput, output)
        in
          if ensure (state, state', input, output, env')
            then (state', env')
            else raise Fail "Ensure failed"
        end

      fun executeSequential initialState actions =
        List.foldl executeUpdateEnsure (initialState, Env.empty) actions

      fun canGenerate state (c as Command {gen, ...}) =
        case gen state of
            NONE => NONE
          | SOME g => SOME (g, c)

      fun genAction commands state scope =
        Gen.bind (Gen.element (List.mapPartial (canGenerate state) commands))
        (fn (gen, Command {exec, cbs, itos, otos, vars, ...}) =>
          let
            val require = Callback.requires cbs
            val update = Callback.updates cbs
            val ensure = Callback.ensures cbs
            val soutput = Var.newSymbolic ()
            val scope' = Scope.insert (scope, soutput)
          in
            Gen.bind (Gen.filter (fn i => require (state, i)) gen)
            (fn input =>
              let
                val state' = update (state, input, Var.symbolic soutput)
                val action = Action {
                    input = input,
                    vars = vars input,
                    soutput = soutput,
                    execute = exec,
                    require = require,
                    update = update,
                    ensure = ensure,
                    itos = itos,
                    otos = otos
                  }
              in
                Gen.pure ((state', scope'), action)
              end)
          end)

      fun dropInvalid (actions, initial) =
        let
          fun loop ([], acc, state, scope) = (List.rev acc, state, scope)
            | loop (step :: steps, acc, state, scope) =
            let
              val Action {input, soutput, vars, require, update, ...} = step
            in
              if require (state, input) andalso Scope.allInScope (scope, vars)
                then
                  let
                    val state' = update (state, input, Var.symbolic soutput)
                    val scope' = Scope.insert (scope, soutput)
                  in
                    loop (steps, step :: acc, state', scope')
                  end
                else loop (steps, acc, state, scope)
            end
        in
          loop (actions, [], initial, Scope.empty)
        end

      (* fun genActions range commands initial = *)
      (*   Gen.list range (genAction commands ) *)

    end

end
