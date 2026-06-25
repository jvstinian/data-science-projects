generic
    N : Positive;
package Reinforcement_Learning.Environments.LineWalk is
    type State_Kind_Type is (Active, Terminal);

    subtype Reward_Type is Integer range -1 .. 1;
    type Player_Type is (Line_Walker);
    type Action_Type is (MoveLeft, MoveRight);
    type Available_Actions_Type is array (Action_Type) of Boolean;
    type Valid_Actions_Type is array (Natural range <>) of Action_Type;
    type Active_State_Type is new Integer range 1 .. N;
    -- type State_Kind_Type is (Active, Terminal);

    type State_Type(Kind : State_Kind_Type := Active) is private;

    function Initial_State return State_Type;
    function Is_Terminal (State : State_Type) return Boolean;
    function Get_Player(State : State_Type) return Player_Type;
    function Step(State : State_Type; Action : Action_Type) return State_Type;
    function Reward(Player: Player_Type; State : State_Type) return Long_Float;
    function Get_Available_Actions (State : State_Type) return Available_Actions_Type;
    function Get_Valid_Actions (State : State_Type) return Valid_Actions_Type;

    procedure Print_State (State : State_Type);

private
    type State_Type(Kind : State_Kind_Type := Active) is
    record
        case Kind is
            when Active =>
                State : Active_State_Type;
            when Terminal =>
                Reward : Reward_Type;
        end case;
    end record;

end Reinforcement_Learning.Environments.LineWalk;

-- initState :: Int -> Int -> LineWalkState
-- initState n pos | pos < 1   = LineWalkState n 1
--                 | pos > n   = LineWalkState n n
--                 | otherwise = LineWalkState n pos
-- 
-- applyAction :: LineWalkAction -> State LineWalkState Reward
-- applyAction MoveLeft = state (\s -> 
--     let LineWalkState n pos = s
--         newpos = pos - 1
--         reward = if newpos < 1
--                  then -1
--                  else 0
--     in (reward, LineWalkState n newpos))
-- applyAction MoveRight = state (\s -> 
--     let LineWalkState n pos = s
--         newpos = pos + 1
--         reward = if newpos > n
--                  then 1
--                  else 0
--     in (reward, LineWalkState n newpos))
-- 
-- instance MCTS.Environment LineWalkState LineWalkAction Double where
--     validActions _ = [MoveLeft, MoveRight]
--     isTerminal (LineWalkState n pos) = (pos < 1) || (pos > n)
--     act s = (flip execState s) . applyAction
--     -- act s a = execState (applyAction a) s
--     reward _ (LineWalkState n pos) =
--         if pos < 1
--         then (-1.0)
--         else if pos > n
--              then 1.0
--              else 0.0
-- 
