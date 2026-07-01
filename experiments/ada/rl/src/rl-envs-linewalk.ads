generic
   N : Positive;
package RL.Envs.LineWalk is
   type State_Kind_Type is (Active, Terminal);

   type Player_Type is (Line_Walker);
   type Action_Type is (MoveLeft, MoveRight);

   type Available_Actions_Type is array (Action_Type) of Boolean;
   type Valid_Actions_Type is array (Natural range <>) of Action_Type;

   type State_Type is private;

   -- Interface for MCTS
   function Initial_State return State_Type;
   function Is_Terminal (State : State_Type) return Boolean;
   function Get_Player(State : State_Type) return Player_Type;
   function Step(State : State_Type; Action : Action_Type) return State_Type;
   function Reward(Player: Player_Type; State : State_Type) return Float;
   function Get_Available_Actions (State : State_Type) return Available_Actions_Type;
   function Get_Valid_Actions (State : State_Type) return Valid_Actions_Type;

   procedure Print_State (State : State_Type);

   -- RL Interface
   type Config_Type is null record;
   type Environment_Type is new Natural range 0 .. N + 1;
   type Observation_Type is new Natural range 1 .. N;
   type Step_Return_Type is record
      Observation: Observation_Type;
      Reward: Float;
      Terminated: Boolean;
   end record;
    
   function Make(Config: Config_Type) return Environment_Type;
   function Reset(Env : in out Environment_Type; Seed_Reset : Seed_Reset_Type) return Observation_Type;
   function Step(Env : in out Environment_Type; action: Action_Type) return Step_Return_Type;
private
   subtype Reward_Type is Integer range -1 .. 1;
   type Active_State_Type is new Integer range 1 .. N;

   type State_Type(Kind : State_Kind_Type := Active) is
   record
       case Kind is
           when Active =>
               State : Active_State_Type;
           when Terminal =>
               Reward : Reward_Type;
       end case;
   end record;

end RL.Envs.LineWalk;
