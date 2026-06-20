with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;
-- with Ada.Numerics.Float_Random; -- use Ada.Numerics.Float_Random;

package Blackjack is
   --  Blackjack is a card game where the goal is to beat the dealer by obtaining cards
   --  that sum to closer to 21 (without going over 21) than the dealers cards.
   -- 
   --  ## Description
   --  The game starts with the dealer having one face up and one face down card,
   --  while the player has two face up cards. All cards are drawn from an infinite deck
   --  (i.e. with replacement).
   -- 
   --  The card values are:
   --  - Face cards (Jack, Queen, King) have a point value of 10.
   --  - Aces can either count as 11 (called a 'usable ace') or 1.
   --  - Numerical cards (2-10) have a value equal to their number.
   -- 
   --  The player has the sum of cards held. The player can request
   --  additional cards (hit) until they decide to stop (stick) or exceed 21 (bust,
   --  immediate loss).
   -- 
   --  After the player sticks, the dealer reveals their facedown card, and draws cards
   --  until their sum is 17 or greater. If the dealer goes bust, the player wins.
   -- 
   --  If neither the player nor the dealer busts, the outcome (win, lose, draw) is
   --  decided by whose sum is closer to 21.
   -- 
   --  This environment corresponds to the version of the blackjack problem
   --  described in Example 5.1 in Reinforcement Learning: An Introduction
   --  by Sutton and Barto [<a href="#blackjack_ref">1</a>].
   -- 
   --  ## Action Space
   --  The action shape is `(1,)` in the range `{0, 1}` indicating
   --  whether to stick or hit.
   -- 
   --  - 0: Stick
   --  - 1: Hit
   -- 
   --  ## Observation Space
   --  The observation consists of a 3-tuple containing: the player's current sum,
   --  the value of the dealer's one showing card (1-10 where 1 is ace),
   --  and whether the player holds a usable ace (0 or 1).
   -- 
   --  The observation is returned as `(int(), int(), int())`.
   -- 
   --  ## Starting State
   --  The starting state is initialised with the following values.
   -- 
   --  | Observation               | Values         |
   --  |---------------------------|----------------|
   --  | Player current sum        |  4, 5, ..., 21 |
   --  | Dealer showing card value |  1, 2, ..., 10 |
   --  | Usable Ace                |  0, 1          |
   -- 
   --  ## Rewards
   --  - win game: +1
   --  - lose game: -1
   --  - draw game: 0
   --  - win game with natural blackjack:
   --  +1.5 (if <a href="#nat">natural</a> is True)
   --  +1 (if <a href="#nat">natural</a> is False)
   -- 
   --  ## Episode End
   --  The episode ends if the following happens:
   -- 
   --  - Termination:
   --  1. The player hits and the sum of hand exceeds 21.
   --  2. The player sticks.
   -- 
   --  An ace will always be counted as usable (11) unless it busts the player.
   -- 
   --  ## Information
   -- 
   --  No additional information is returned.
   -- 
   --  ## Arguments
   -- 
   --  ```python
   --  import gymnasium as gym
   --  gym.make('Blackjack-v1', natural=False, sab=False)
   --  ```
   -- 
   --  <a id="nat"></a>`natural=False`: Whether to give an additional reward for
   --  starting with a natural blackjack, i.e. starting with an ace and ten (sum is 21).
   -- 
   --  <a id="sab"></a>`sab=False`: Whether to follow the exact rules outlined in the book by
   --  Sutton and Barto. If `sab` is `True`, the keyword argument `natural` will be ignored.
   --  If the player achieves a natural blackjack and the dealer does not, the player
   --  will win (i.e. get a reward of +1). The reverse rule does not apply.
   --  If both the player and the dealer get a natural, it will be a draw (i.e. reward 0).
   -- 
   --  ## References
   --  <a id="blackjack_ref"></a>[1] R. Sutton and A. Barto, “Reinforcement Learning:
   --  An Introduction” 2020. [Online]. Available: [http://www.incompleteideas.net/book/RLbook2020.pdf](http://www.incompleteideas.net/book/RLbook2020.pdf)
   -- 
   --  ## Version History
   --  * v1: Fix the natural handling in Blackjack
   --  * v0: Initial version release
   
   type Card_Type is (Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King);
   subtype Card_Value_Type is Integer range 1..10;
   type Card_Value_Array is array (Card_Type) of Card_Value_Type;

   Card_Values : constant Card_Value_Array := (
      Ace => 1, Two => 2, Three => 3, Four => 4, Five => 5,
      Six => 6, Seven => 7, Eight => 8, Nine => 9, Ten => 10,
      Jack => 10, Queen => 10, King => 10);
  
   -- Rather than using a dynamic length array for the hand which
   -- would require using a discriminant with the environment type, and
   -- rather than using a fixed length array in combination with a variable
   -- to track the number of cards in the hand, we instead simply track
   -- the count of each card type in the hand.
   -- This is sufficient to compute the sum of the hand and the other required properties.
   type Hand_Type is array (Card_Type) of Natural;

   package Draw_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Card_Type);

   type Action_Type is (Stick, Hit);

   type Natural_Win_Reward_Type is (SAB, Natural_Win, No_Natural_Win);
   type Config_Type is record
      Natural_Win_Reward : Natural_Win_Reward_Type;
      Auto_Hit : Boolean;
   end record;

   type Environment_Type is limited private;

   type Observation_Type is record
      Player_Sum : Integer;  -- TODO: Natural?
      Dealer_Showing_Card_Value : Card_Value_Type;
      Usable_Ace : Boolean;
   end record;
   
   type Step_Return_Type is record
      Observation: Observation_Type;
      Reward: Float;
      Terminated: Boolean;
      Truncated: Boolean;
   end record;
   
   function Make(Config: Config_Type) return Environment_Type;
   function Reset(Env : in out Environment_Type) return Observation_Type;
   function Step(Env : in out Environment_Type; Action : Action_Type) return Step_Return_Type;
   procedure Render_Text(Env : Environment_Type);
private
   type Hand_Summary_Type is record
      Hand_Sum : Integer;
      Usable_Ace : Boolean;
   end record;

   -- Note that we omit the dealer_top_card_suit and dealer_top_card_value_str 
   type Environment_Type is record
      -- Natural_Win_Reward : Natural_Win_Reward_Type;
      Config : Config_Type;
      Gen : Draw_Random.Generator;
      Player_Hand : Hand_Type;
      Dealer_Hand : Hand_Type;
      Dealer_Showing_Card : Card_Type;
   end record;
   
   function Draw_Card(Gen : in out Draw_Random.Generator) return Card_Type;
   function Draw_Hand(Gen : in out Draw_Random.Generator) return Hand_Type;
   function Hand_Sum_And_Usable_Ace(Hand : Hand_Type) return Hand_Summary_Type;
   function Sum_Hand(Hand : Hand_Type) return Integer;
   function Get_Obs(Env : Environment_Type) return Observation_Type;

end Blackjack;
