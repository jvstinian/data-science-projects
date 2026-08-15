with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Numerics.Discrete_Random;
with RL; use RL;  -- seed reset
with RL.Envs.Cartpole; use RL.Envs.Cartpole;

procedure Cartpole_Main is
    package Action_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
    Gen: Action_Random.Generator;
    Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default);
    Config: Config_Type := Config_Type'(others => <>); -- use defaults
    Env: Environment_Type := Make(Config);
    State: Observation_Type;
    Action: Action_Type;
    Step_Output: Step_Return_Type;
    I: Integer;
    Total_Reward: Float := 0.0;
begin
    Action_Random.Reset(Gen);

    -- if Get_Sutton_Barto_Reward then
    --     Put_Line("Using Sutton-Barto reward");
    -- else
    --     Put_Line("Using default reward");
    -- end if;

    Put_Line("Initializing Cartpole environment");
    State := Reset(Env, Seed_Reset);
    I := 0;
    loop
        Action := Action_Random.Random(Gen);
        Put_Line("Calling Step with action " & Action'Image);
        Step_Output := Step(Env, Action);
        State := Step_Output.Observation;
        Total_Reward := Total_Reward + Step_Output.Reward;
        I := I + 1;
        -- Print the reward for this step
        Put("Step" & I'Image & " Reward: ");
        Ada.Float_Text_IO.Put(Item => Step_Output.Reward, Fore => 2, Aft => 2, Exp => 0);
        New_Line;
        if Step_Output.Terminated then
            Put("Total Reward: ");
            Ada.Float_Text_IO.Put(Item => Total_Reward, Fore => 2, Aft => 2, Exp => 0);
            New_Line;
            Put_Line("Terminated: " & Step_Output.Terminated'Image);
            exit;
        end if;
    end loop;
end Cartpole_Main;
