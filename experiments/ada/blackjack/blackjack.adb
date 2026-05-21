package body Blackjack is
   function Draw_Card(Gen : in out Draw_Random.Generator) return Card_Type is
      Card : Card_Type := Draw_Random.Random(Gen);
   begin
      return Card;
   end Draw_Card;

   function Draw_Hand(Gen : in out Draw_Random.Generator) return Hand_Type is
      Hand : Hand_Type := (others => 0);
      First_Card : Card_Type := Draw_Card(Gen);
      Second_Card : Card_Type := Draw_Card(Gen);
   begin
      Hand(First_Card) := Hand(First_Card) + 1;  -- Should always be 1
      Hand(Second_Card) := Hand(Second_Card) + 1;
      return Hand;
   end Draw_Hand;

   function Usable_Ace(Hand: Hand_Type) return Boolean is
      Has_Ace : Boolean := Hand(Ace) > 0;
      Hand_Sum : Integer := 0;
   begin
      if Has_Ace then
         for Card in Card_Type loop
            Hand_Sum := Hand_Sum + Hand(Card) * Card_Values(Card);
         end loop;
         Hand_Sum := Hand_Sum + 10;
         if Hand_Sum <= 21 then
            return True;
         else
            return False;
         end if;
      else
         return False;
      end if;
   end Usable_Ace;

   -- Helper to compute both sum with and without ace counted as 11,
   -- and whether the hand has a usable ace.
   function Hand_Sum_And_Usable_Ace(Hand : Hand_Type) return Hand_Summary_Type is
      Hand_Sum : Integer := 0;
      Res : Hand_Summary_Type;
   begin
      for Card in Card_Type loop
         Hand_Sum := Hand_Sum + Hand(Card) * Card_Values(Card);
      end loop;

      if (Hand(Ace) > 0) and then Hand_Sum + 10 <= 21 then
         Res.Hand_Sum := Hand_Sum + 10;
         Res.Usable_Ace := True;
      else
         Res.Hand_Sum := Hand_Sum;
         Res.Usable_Ace := False;
      end if;
      return Res;
   end Hand_Sum_And_Usable_Ace;

   function Get_Obs(Env : Environment_Type) return Observation_Type is
      Temp_Summary : Hand_Summary_Type := Hand_Sum_And_Usable_Ace(Env.Player_Hand);
   begin
      return Observation_Type'(
         Player_Sum => Temp_Summary.Hand_Sum,
         Dealer_Showing_Card => Env.Dealer_Showing_Card,
         Usable_Ace => Temp_Summary.Usable_Ace);
   end Get_Obs;

   function Make(Config: Config_Type) return Environment_Type is
      -- Env : Environment_Type := (
      --    Natural_Win_Reward => Config.Natural_Win_Reward,
      --    Player_Hand => (others => 0),
      --    Dealer_Hand => (others => 0),
      --    Dealer_Showing_Card => Ace); -- TODO: What to do for the generator?
   begin
      return Environment_Type'(
         Natural_Win_Reward => Config.Natural_Win_Reward,
         Player_Hand => (others => 0),
         Dealer_Hand => (others => 0),
         Dealer_Showing_Card => Ace,
         Gen => <>); -- TODO: What to do for the generator?
   end Make;

   function Reset(Env : in out Environment_Type) return Observation_Type is
      Dealer_Card : Card_Type;
      Other_Dealer_Card : Card_Type;
      Player_Sum : Integer;
      Usable_Ace : Boolean;
   begin
      -- TODO: Need to determine how we want to set a seed
      Draw_Random.Reset(Env.Gen);
      -- Reset dealer hand
      Env.Dealer_Hand := (others => 0);
      Dealer_Card := Draw_Card(Env.Gen);
      Other_Dealer_Card := Draw_Card(Env.Gen);
      Env.Dealer_Hand(Dealer_Card) := 1; -- The first card, should only be 1 card
      Env.Dealer_Hand(Other_Dealer_Card) := Env.Dealer_Hand(Other_Dealer_Card) + 1;
      Env.Dealer_Showing_Card := Dealer_Card;
      
      -- Reset player hand
      Env.Player_Hand := Draw_Hand(Env.Gen);

      return Get_Obs(Env);
   end Reset;
--     def reset(
--         self,
--         seed: int | None = None,
--         options: dict | None = None,
--     ):
--         super().reset(seed=seed)
--         self.dealer = draw_hand(self.np_random)
--         self.player = draw_hand(self.np_random)
-- 
--         if self.render_mode == "human":
--             self.render()
--         return self._get_obs(), {}

   function Step(Env : in out Environment_Type; Action : Action_Type) return Step_Return_Type is
      -- Helper functions
      function Sum_Hand(Hand : Hand_Type) return Integer is
      begin
         return Hand_Sum_And_Usable_Ace(Hand).Hand_Sum;
      end Sum_Hand;

      function Is_Bust(Hand : Hand_Type) return Boolean is
      begin
         return Sum_Hand(Hand) > 21;
      end Is_Bust;

      function Score(Hand : Hand_Type) return Integer is
      begin
            if Is_Bust(Hand) then
               return 0;
            else
               return Sum_Hand(Hand);
            end if;
      end Score;

      function Cmp(A : Integer; B : Integer) return Float is
      begin
         if A > B then
            return 1.0;
         elsif A < B then
            return -1.0;
         else
            return 0.0;
         end if;
      end Cmp;

      function Is_Natural(Hand : Hand_Type) return Boolean is
         Has_One_10 : Boolean := ((Hand(Ten) + Hand(Jack) + Hand(Queen) + Hand(King)) = 1);
      begin
         return Hand(Ace) = 1 and then Has_One_10;
      end Is_Natural;
      -- The Python code uses a method cmp to assign a reward, which
      -- function Player_Has_Higher_Score(Env : Environment_Type) return Boolean is
      --    A : Integer := Score(Env.Player_Hand);
      --    B : Integer := Score(Env.Dealer_Hand);
      -- begin
      --    if A > B then
      --       return True;
      --    else
      --       return False;
      --    end if;
      -- end Player_Has_Higher_Score;

      -- Temporary variables
      Reward : Float := 0.0;
      Terminated : Boolean := False;
      New_Card : Card_Type;
   begin
      case Action is
         when Hit =>
            New_Card := Draw_Card(Env.Gen);
            Env.Player_Hand(New_Card) := Env.Player_Hand(New_Card) + 1;
            if Is_Bust(Env.Player_Hand) then
               Terminated := True;
               Reward := -1.0;
            else
               Terminated := False;
               Reward := 0.0;
            end if;
         when Stick =>
            Terminated := True;
            while Sum_Hand(Env.Dealer_Hand) < 17 loop
               New_Card := Draw_Card(Env.Gen);
               Env.Dealer_Hand(New_Card) := Env.Dealer_Hand(New_Card) + 1;
            end loop;
            Reward := Cmp(Score(Env.Player_Hand), Score(Env.Dealer_Hand));
            if Env.Natural_Win_Reward = SAB and then Is_Natural(Env.Player_Hand) and then not Is_Natural(Env.Dealer_Hand) then
               -- Player automatically wins. Rules consistent with S&B
               Reward := 1.0;
            elsif Env.Natural_Win_Reward = Natural and then Is_Natural(Env.Player_Hand) and then Reward = 1.0 then
               -- Natural gives extra points, but doesn't autowin. Legacy implementation
               Reward := 1.5;
            end if;
      end case;
      return Step_Return_Type'(
         Observation => Get_Obs(Env),
         Reward => Reward,
         Terminated => Terminated,
         Truncated => False);
   end Step;
--     def step(self, action):
--         assert self.action_space.contains(action)
--         if action:  # hit: add a card to players hand and return
--             self.player.append(draw_card(self.np_random))
--             if is_bust(self.player):
--                 terminated = True
--                 reward = -1.0
--             else:
--                 terminated = False
--                 reward = 0.0
--         else:  # stick: play out the dealers hand, and score
--             terminated = True
--             while sum_hand(self.dealer) < 17:
--                 self.dealer.append(draw_card(self.np_random))
--             reward = cmp(score(self.player), score(self.dealer))
--             if self.sab and is_natural(self.player) and not is_natural(self.dealer):
--                 # Player automatically wins. Rules consistent with S&B
--                 reward = 1.0
--             elif (
--                 not self.sab
--                 and self.natural
--                 and is_natural(self.player)
--                 and reward == 1.0
--             ):
--                 # Natural gives extra points, but doesn't autowin. Legacy implementation
--                 reward = 1.5
-- 
--         if self.render_mode == "human":
--             self.render()
--         # truncation=False as the time limit is handled by the `TimeLimit` wrapper added during `make`
--         return self._get_obs(), reward, terminated, False, {}
-- 
   procedure Render_Text(Env : Environment_Type) is
      Obs : Observation_Type := Get_Obs(Env);
   begin
      Put("Player Sum: " & Obs.Player_Sum'Image & ", ");
      Put("Usable Ace: " & Obs.Usable_Ace'Image & ", ");
      Put("Dealer Showing Card: " & Obs.Dealer_Showing_Card'Image);
      New_Line;
   end Render_Text;
end Blackjack;

-- import numpy as np
-- 
-- def cmp(a, b):
--     return float(a > b) - float(a < b)
-- 
-- 
-- # 1 = Ace, 2-10 = Number cards, Jack/Queen/King = 10
-- deck = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10]
-- 
-- 
-- def draw_hand(np_random):
--     return [draw_card(np_random), draw_card(np_random)]
-- 
-- 
-- def usable_ace(hand):  # Does this hand have a usable ace?
--     return int(1 in hand and sum(hand) + 10 <= 21)
-- 
-- 
-- def sum_hand(hand):  # Return current hand total
--     if usable_ace(hand):
--         return sum(hand) + 10
--     return sum(hand)
--
-- 
-- def is_bust(hand):  # Is this hand a bust?
--     return sum_hand(hand) > 21
-- 
-- 
-- def score(hand):  # What is the score of this hand (0 if bust)
--     return 0 if is_bust(hand) else sum_hand(hand)
-- 
-- 
-- def is_natural(hand):  # Is this hand a natural blackjack?
--     return sorted(hand) == [1, 10]
-- 
-- 
