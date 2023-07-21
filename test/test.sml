
structure Test =
struct
  open Hedgehog

  structure Turnstile =
    struct
      datatype state = Locked | Unlocked

      type t = state ref

      fun new () = ref Locked

      fun insertCoin t = t := Unlocked

      fun push t =
        case !t of
            Locked => false
          | Unlocked => true
    end

  structure TurnstileModel =
    struct
      datatype state = TLocked | TUnlocked

      val initial = TLocked

      datatype coin = Coin

      datatype push = Push

      (* This tests the "insertCoin" command
       * We can always insert a coin, so no preconditions
       * Postcondition is that the state is in the unlocked position
       * The ensure callback updates the model with the actual state
       *
       * 'input = coin
       * 'output = ()
       * 'state = state
       *)
      fun s_coin t = State.Command.new {
          generate =
            fn state_ => SOME (Gen.pure Coin),
          vars =
            fn Coin => [],
          execute =
            fn (Coin, env_) => Turnstile.insertCoin t,
          callbacks = [
            State.Callback.update (fn (state_, Coin, out_) =>
              TUnlocked),
            State.Callback.ensure (fn (prev_, cur, Coin, (), env_) =>
              cur = TUnlocked)
          ],
          inputToString =
            fn Coin => "Coin",
          outputToString =
            fn () => "()"
        }

      val _ : Turnstile.t -> state State.Command.t = s_coin

      (* This tests the "push" command when the turnstile is locked
       *
       *)
      fun s_push_locked t = State.Command.new {
          generate =
            fn TLocked => SOME (Gen.pure Push)
             | TUnlocked => NONE,
          vars =
            fn Push => [],
          execute =
            fn (Push, env_) => Turnstile.push t,
          callbacks = [
            State.Callback.require (fn (state, Push) =>
              state = TLocked),
            State.Callback.update (fn (state_, Push, out_) =>
              TLocked),
            State.Callback.ensure (fn (prev, cur, Push, output, env_) =>
              prev = TLocked
              andalso cur = TLocked
              andalso output = false)
          ],
          inputToString =
            fn Push => "Push",
          outputToString =
            fn b => Bool.toString b
        }

      (* This tests the "push" command when the turnstile is unlocked
       *
       *)
      fun s_push_unlocked t = State.Command.new {
          generate =
            fn TLocked => NONE
             | TUnlocked => SOME (Gen.pure Push),
          vars =
            fn Push => [],
          execute =
            fn (Push, env_) => Turnstile.push t,
          callbacks = [
            State.Callback.require (fn (state, Push) =>
              state = TUnlocked),
            State.Callback.update (fn (state_, Push, out_) =>
              TUnlocked),
            State.Callback.ensure (fn (prev, cur, Push, output, env_) =>
              prev = TUnlocked
              andalso cur = TUnlocked
              andalso output = true)
          ],
          inputToString =
            fn Push => "Push",
          outputToString =
            fn b => Bool.toString b
        }
      
    end

  structure Erlang =
    struct
      open State

      datatype pid = Pid of int
      datatype name = Name of string

      val pidRef = ref 0

      fun ioSpawn () =
        Pid (!pidRef) before pidRef := !pidRef + 1




      datatype state = State of {
          pids : pid Var.t list,
          regs : (name * pid Var.t) list
        }

      val initialState = State {pids = [], regs = []}

      datatype spawn = Spawn

      val spawn = Command.new {
          generate =
            fn state_ => SOME (Gen.pure Spawn),
          vars =
            fn Spawn => [],
          execute =
            fn (Spawn, env_) => ioSpawn (),
          callbacks = [
            Callback.update (fn (state, Spawn, output) =>
              let
                val State {pids, regs} = state
              in
                State {pids = output :: pids, regs = regs}
              end)
          ],
          inputToString =
            fn Spawn => "Spawn",
          outputToString =
            fn Pid n => "Pid " ^ Int.toString n
        }
    end

end
