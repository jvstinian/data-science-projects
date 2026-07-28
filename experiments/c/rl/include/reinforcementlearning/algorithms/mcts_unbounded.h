#include <reinforcementlearning/envs/linewalk.h>

/*
with Ada.Finalization;
with Ada.Containers; -- use Ada.Containers;
with Ada.Containers.Vectors;
with Environment_State; use Environment_State;


generic
    type State_Type (Kind: State_Kind_Type) is private;
    -- type State_Type is private;  -- NOTE: This works
    -- type State_Kind_Type is (<>);
    -- Active_Kind_Value : State_Kind_Type;
    type Action_Type is (<>);
    type Player_Type is (<>);

    -- type Available_Actions_Type is array (Action_Type) of Boolean;
    type Valid_Actions_Type is array (Natural range <>) of Action_Type;

    with function Initial_State return State_Type;
    with function Is_Terminal (State : State_Type) return Boolean;
    with function Get_Player(State : State_Type) return Player_Type;
    with function Step(State : State_Type; Action : Action_Type) return State_Type;
    with function Reward(Player: Player_Type; State : State_Type) return Long_Float;
    -- with function Get_Available_Actions (State : State_Type) return Available_Actions_Type;
    with function Get_Valid_Actions (State : State_Type) return Valid_Actions_Type;
*/

enum TreeKind {
    /* Root_Node, */
    Node,
    Leaf/*,
    Unexplored_Action_Placeholder */ /* Not needed yet */
};

struct Tree;

/*
struct RootNode {
    unsigned int num_visits;
    struct State root_state;
    \/\*
    Root_Actions2 : Controlled_Node_Array_Access;
    \*\/
};
*/

struct ActionReward {
    enum LineWalkAction action;
    float reward;
};

struct Node {
    unsigned int num_visits;
    /* enum Action action; */
    /* enum Player parent_player; */
    struct LineWalkState node_state;
    /* float total_reward; */
    unsigned int num_actions;
    struct ActionReward* ars;
    unsigned int actions_tried;
    struct Tree* trees;
    /*
    Node_Actions : Controlled_Node_Array_Access ; -- Controlled_Tree_Vectors.Vector;
    Probably implement as Tree*
    TODO: Need to figure out what to do about the Preserved flag
    */
};

struct Leaf {
    unsigned int num_visits;
    /*
    enum LineWalkAction action;
    enum LineWalkPlayer parent_player;
    */
    struct LineWalkState terminal_state;
    /*
    float terminal_reward;
    */
};

struct UnexploredAction {
    enum LineWalkAction unexplored_action;
};

union TreeData {
    struct Node node;
    struct Leaf leaf;
    struct UnexploredAction ua;
};

struct Tree {
    enum TreeKind tag;
    union TreeData data;
};
    
struct UCTParams {
    float exploration_const;
};

struct UCTParams get_selection_uct_params(struct UCTParams params);

void uct_update(struct UCTParams params, struct Tree* t);

struct Tree* mcts_tree_new();

int uct_main();

/*
package MCTS_Unbounded is

    -- package Action_Vectors is new Ada.Containers.Vectors
    --     (Index_Type   => Natural,
    --      Element_Type => Action_Type);

    type Tree;
    -- type Tree is private;
    -- type Tree_Kind_Type is (Root_Node, Node, Leaf, Unexplored_Action_Placeholder);
    -- type Tree (Kind : Tree_Kind_Type) is private;
    type ControlledTree;
    type Tree_Access is access Tree;
 
    type Tree_Kind_Type is (Root_Node, Node, Leaf, Unexplored_Action_Placeholder);
    
    type UCT_Parameters is
    record
        null;
    end record;

    type ControlledTree is new Ada.Finalization.Controlled with
    record
        Preserve: Boolean := False;
        Acctree: Tree_Access;
    end record;
        
    -- Declaration must follow the record definition otherwise we encounter the error
    -- "this primitive operation is declared too late"
    overriding
    procedure Initialize (E : in out ControlledTree);
    
    overriding
    procedure Adjust (E : in out ControlledTree);
    
    overriding
    procedure Finalize (E : in out ControlledTree);

    -- TODO: Probably just use Initialize to create the initial tree
    --       If we go that route, this function can be removed
    function Initialize_MCTS_Tree return ControlledTree;
    function Initialize_Node (Parent_State: State_Type; Action: Action_Type; New_State: State_Type; Reward: Long_Float) return ControlledTree;
    function Initialize_Unexplored_Action_Leaf (Action: Action_Type) return ControlledTree;
    
    procedure Print_Controlled_Tree_Summary (T: ControlledTree);
    procedure UCT_Select(T: in out ControlledTree);

    package Controlled_Tree_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => ControlledTree);
    -- use Controlled_Expr_Vectors;

    type Controlled_Node_Array is array (Natural range <>) of ControlledTree;
    type Controlled_Node_Array_Access is access Controlled_Node_Array;
    
    -- subtype Tree_Root_Node is Tree (Root_Node);
    function Default_Policy_Terminal_State (State : State_Type) return State_Type;

    type UCT_Status is (Ok, Unexplored_Action_Leaf_Reached);
    type UCT_Result(Kind : UCT_Status) is
    record
        case Kind is
            when Ok =>
                State : State_Type(Terminal);
            when Unexplored_Action_Leaf_Reached =>
                null;
        end case;
    end record;


end MCTS_Unbounded;
*/
