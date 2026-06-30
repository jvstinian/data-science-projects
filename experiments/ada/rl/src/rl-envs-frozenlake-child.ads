generic
   Map_Info : Map_Info_Type;
package RL.Envs.Frozenlake.Child is
    -- function Get_Map_Rows return Positive;
    -- function Get_Map_Cols return Positive;

    Num_Rows : Positive := Map_Info.Rows;
    Num_Cols : Positive := Map_Info.Cols;

    procedure Dummy_Method;

    type Alt_Discrete_State_Type is new Natural range 0 .. (Num_Rows * Num_Cols - 1);
    type Precise_State_Type is new Natural range 0 .. (Num_Rows * Num_Cols - 1);
    type Precise_Model_Type is array (Precise_State_Type, Action_Type, Precise_State_Type) of Transition_Probability_Type;
    function Get_Model(Config: Environment_Config) return Precise_Model_Type;
end RL.Envs.Frozenlake.Child;
