package body RL.Envs.Blackjack is
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
      Hand_Sum : Natural := 0;
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
      
   function Sum_Hand(Hand : Hand_Type) return Integer is
   begin
      return Hand_Sum_And_Usable_Ace(Hand).Hand_Sum;
   end Sum_Hand;

   function Get_Obs(Env : Environment_Type) return Observation_Type is
      Temp_Summary : Hand_Summary_Type := Hand_Sum_And_Usable_Ace(Env.Player_Hand);
   begin
      return Observation_Type'(
         Player_Sum => Temp_Summary.Hand_Sum,
         Dealer_Showing_Card_Value => Card_Values(Env.Dealer_Showing_Card),
         Usable_Ace => Temp_Summary.Usable_Ace);
   end Get_Obs;

   function Make(Config: Config_Type) return Environment_Type is
   begin
      return Environment_Type'(
         Config => Config,
         Player_Hand => (others => 0),
         Dealer_Hand => (others => 0),
         Dealer_Showing_Card => Ace,  -- placeholder till reset is called
         Gen => <>);
   end Make;

   function Reset (Env : in out Environment_Type; Seed_Reset : Seed_Reset_Type)
      return Observation_Type is
      -- Helper function
      procedure Hit_Till_12(Hand : in out Hand_Type) is
         New_Card : Card_Type;
      begin
         while Sum_Hand(Hand) < 12 loop
            New_Card := Draw_Card(Env.Gen);
            Hand(New_Card) := Hand(New_Card) + 1;
         end loop;
      end Hit_Till_12;

      Dealer_Card : Card_Type;
      Other_Dealer_Card : Card_Type;
      Usable_Ace : Boolean;
   begin
      case Seed_Reset.Kind is
         when Set_Default => Draw_Random.Reset(Env.Gen);
         when No_Set      => null;
         when Set_Seed    => Draw_Random.Reset(Env.Gen, Seed_Reset.Seed);
      end case;
      -- Reset dealer hand
      Env.Dealer_Hand := (others => 0);
      Dealer_Card := Draw_Card(Env.Gen);
      Other_Dealer_Card := Draw_Card(Env.Gen);
      Env.Dealer_Hand(Dealer_Card) := 1; -- The first card, should only be 1 card
      Env.Dealer_Hand(Other_Dealer_Card) := Env.Dealer_Hand(Other_Dealer_Card) + 1;
      Env.Dealer_Showing_Card := Dealer_Card;
      
      -- Reset player hand
      Env.Player_Hand := Draw_Hand(Env.Gen);
      
      if Env.Config.Auto_Hit then
         Hit_Till_12(Env.Player_Hand);
      end if;

      return Get_Obs(Env);
   end Reset;

   function Step(Env : in out Environment_Type; Action : Action_Type) return Step_Return_Type is
      -- Helper functions
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
            if Env.Config.Natural_Win_Reward = SAB and then Is_Natural(Env.Player_Hand) and then not Is_Natural(Env.Dealer_Hand) then
               -- Player automatically wins. Rules consistent with S&B
               Reward := 1.0;
            elsif Env.Config.Natural_Win_Reward = Natural_Win and then Is_Natural(Env.Player_Hand) and then Reward = 1.0 then
               -- Natural gives extra points, but doesn't autowin. Legacy implementation
               Reward := 1.5;
            end if;
      end case;
      return Step_Return_Type'(
         Observation => Get_Obs(Env),
         Reward => Reward,
         Terminated => Terminated
      );
   end Step;

   procedure Render_Text(Env : Environment_Type) is
      Obs : Observation_Type := Get_Obs(Env);
   begin
      Put("Player Sum: " & Obs.Player_Sum'Image & ", ");
      Put("Usable Ace: " & Obs.Usable_Ace'Image & ", ");
      Put("Dealer Showing Card Value: " & Obs.Dealer_Showing_Card_Value'Image);
      Put("(" & Env.Dealer_Showing_Card'Image & ")");
      New_Line;
   end Render_Text;
end RL.Envs.Blackjack;
