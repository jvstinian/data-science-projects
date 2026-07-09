with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;

--  ## Description
--  Blackjack is a card game where the goal is to beat the dealer by obtaining
--  cards that sum to closer to 21 (without going over 21) than the dealers
--  cards.
-- 
--  The game starts with the dealer having one face up and one face down card,
--  while the player has two face up cards. All cards are drawn from an
--  infinite deck (i.e. with replacement).
-- 
--  The card values are:
--  - Face cards (Jack, Queen, King) have a point value of 10.
--  - Aces can either count as 11 (called a 'usable ace') or 1.
--  - Numerical cards (2-10) have a value equal to their number.
-- 
--  The player has the sum of cards held. The player can request
--  additional cards (hit) until they decide to stop (stick) or exceed 21
--  (bust, immediate loss).
-- 
--  After the player sticks, the dealer reveals their facedown card, and draws
--  cards until their sum is 17 or greater. If the dealer goes bust, the
--  player wins.
-- 
--  If neither the player nor the dealer busts, the outcome (win, lose, draw)
--  is decided by whose sum is closer to 21.
-- 
--  This environment corresponds to the version of the blackjack problem
--  described in Example 5.1 in Reinforcement Learning: An Introduction
--  by Sutton and Barto.
-- 
--  ## Action Space
--  The action space consists of two discrete actions, STICK or HIT.
-- 
--  ## Observation Space
--  The observation consists of a fields containing
--  * the player's current sum,
--  * the value of the dealer's one showing card (1-10 where 1 is ace),
--  * and whether the player holds a usable ace (boolean).
-- 
--  ## Starting State
--  In this implementation, the starting state depends on a configuration
--  value called Auto_Hit.  
--
--  If Auto_Hit is False, then the player starts with two cards and
--  starts with a sum of 4 or greater.
--
--  If Auto_Hit is True, then we assume the player
--  automatically hits till their sum is at least 12.
--
--  Thus, the player's current sum in the starting state
--  is between 4 and 21 inclusive if Auto_Hit is false,
--  and between 12 and 21 inclusive if Auto_Hit is true.
--
--  The other two values in the starting state do not depend
--  on the Auto_Hit configuration value.
--
--  The dealer's showing card value is between 1 and 10 inclusive
--
--  Whether the player has a usable ace is indicated by a boolean value.
-- 
--  | Observation               | Values (No Auto_Hit) | Values (Auto_Hit) | 
--  |---------------------------|----------------------|-------------------|
--  | Player current sum        |  4, 5, ..., 21       | 12, 13, ..., 21   |
--  | Dealer showing card value |  1, 2, ..., 10       | 1, 2, ..., 10     |
--  | Usable Ace                |  False, True         | False, True       |
-- 
--  ## Rewards
--  The reward depends on the configuration value called Natural_Win_Reward.
--  This configuration value can take the values
--  * SAB (stands for Sutton & Barto),
--  * Natural_Win, and
--  * No_Natural_Win
--  
--  The reward values are as follows when No_Natural_Win is used:
--  * win game: +1
--  * lose game: -1
--  * draw game: 0
--
--  The following adjustments are made for natural hands for the other
--  configuration values: 
--  * For SAB, if the player has a natural blackjack and the dealer does not,
--    the player automatically wins with a reward of +1,
--  * For Natural_Win, if the player has a natural blackjack and the dealer does
--    not have 21, then the player wins with a reward of +1.5
--    (i.e. an extra 0.5 points for the natural).
--
--  ## Episode End
--  The episode ends if the following happens:
--  1. The player hits and the sum of hand exceeds 21 (bust), or
--  2. The player sticks.
-- 
--  An ace will always be counted as usable (11) unless it busts the player.
-- 
--  ## Environment Configuration
--  The `Make` command can be called with a value `Config` of `Config_Type`
--  to specify the configuration of the environment.  As described above,
--  there are two configuration values, `Natural_Win_Reward` and `Auto_Hit`.
--  `Natural_Win_Reward` affects how rewards are calculated for natural hands,
--  as described in the section on rewards above, while the
--  `Auto_Hit` configuration affects the starting state of the player, as
--  described in the section on starting state above.
-- 
--  ## References
--  [1] R. Sutton and A. Barto, “Reinforcement Learning: An Introduction”
--  2020. [Online]. Available:
--  [http://www.incompleteideas.net/book/RLbook2020.pdf](http://www.incompleteideas.net/book/RLbook2020.pdf)
--
--  ## Differences With The Python Implementation
--  Some differences with the Python Gymnasium implementation include:
--  * we combine the `natural` and `sab` configuration values into a single
--    discrete configuration value called `Natural_Win_Reward`, 
--  * we add the `Auto_Hit` configuration value to control the starting state
--    of the player, which was introduced to facilitate comparison of results
--    with the Sutton and Barto book.
package RL.Envs.Blackjack is
   type Natural_Win_Reward_Type is (SAB, Natural_Win, No_Natural_Win);
   type Config_Type is record
      Natural_Win_Reward : Natural_Win_Reward_Type;
      Auto_Hit : Boolean;
   end record;

   type Action_Type is (Stick, Hit);

   type Environment_Type is limited private;

   subtype Card_Value_Type is Natural range 1..10;
   
   type Observation_Type is record
      Player_Sum : Natural;
      Dealer_Showing_Card_Value : Card_Value_Type;
      Usable_Ace : Boolean;
   end record;
   
   type Step_Return_Type is record
      Observation: Observation_Type;
      Reward: Float;
      Terminated: Boolean;
   end record;
   
   function Make(Config: Config_Type) return Environment_Type;
   function Reset(Env : in out Environment_Type; Seed_Reset : Seed_Reset_Type)
      return Observation_Type;
   function Step(Env : in out Environment_Type; Action : Action_Type) return Step_Return_Type;
   procedure Render_Text(Env : Environment_Type);
private
   type Card_Type is (Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King);
   type Card_Value_Array is array (Card_Type) of Card_Value_Type;

   -- Rather than using a dynamic length array for the hand which
   -- would require using a discriminant with the environment type, and
   -- rather than using a fixed length array in combination with a variable
   -- to track the number of cards in the hand, we instead simply track
   -- the count of each card type in the hand.
   -- This is sufficient to compute the sum of the hand and the other required properties.
   type Hand_Type is array (Card_Type) of Natural;

   Card_Values : constant Card_Value_Array := (
      Ace => 1, Two => 2, Three => 3, Four => 4, Five => 5,
      Six => 6, Seven => 7, Eight => 8, Nine => 9, Ten => 10,
      Jack => 10, Queen => 10, King => 10);
  
   package Draw_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Card_Type);

   type Hand_Summary_Type is record
      Hand_Sum : Natural;
      Usable_Ace : Boolean;
   end record;

   type Environment_Type is record
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
end RL.Envs.Blackjack;
