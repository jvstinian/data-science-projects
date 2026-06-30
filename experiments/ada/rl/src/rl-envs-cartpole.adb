with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body RL.Envs.Cartpole is
    function Get_Sutton_Barto_Reward return Boolean is
    begin
        return Sutton_Barto_Reward;
    end Get_Sutton_Barto_Reward;
    
    procedure Set_Sutton_Barto_Reward(Use_Sutton_Barto_Reward: Boolean) is
    begin
        Sutton_Barto_Reward := Use_Sutton_Barto_Reward;
    end Set_Sutton_Barto_Reward;
    
    function Get_Kinematics_Integrator return Kinematics_Integrator_Type is
    begin
        return Kinematics_Integrator;
    end Get_Kinematics_Integrator;

    procedure Set_Kinematics_Integrator (K: Kinematics_Integrator_Type) is
    begin
        Kinematics_Integrator := K;
    end Set_Kinematics_Integrator;

    function Make(Config: Config_Type) return Environment_Type is
    begin
      return Environment_Type'(
         Config => Config,
         Gen => <>,
         State => Observation_Type'(
            X => 0.0, X_Dot => 0.0, Theta => 0.0, Theta_Dot => 0.0
         )
      );
    end Make;

   function Reset(Env : in out Environment_Type; Seed_Reset : Seed_Reset_Type) return Observation_Type is
      Low: constant Float := -0.05;
      High: constant Float := 0.05;
      High_Low_Diff: constant Float := High - Low;
   begin
      case Seed_Reset.Kind is
         when Set_Default => Float_Random.Reset(Env.Gen);
         when No_Set      => null;
         when Set_Seed    => Float_Random.Reset(Env.Gen, Seed_Reset.Seed);
      end case;
      Env.State := Observation_Type'(
            X => Threshold_Type(Low + High_Low_Diff * Float_Random.Random(Env.Gen)),
            X_Dot => Low + High_Low_Diff * Float_Random.Random(Env.Gen),
            Theta => Theta_Threshold_Type(Low + High_Low_Diff * Float_Random.Random(Env.Gen)),
            Theta_Dot => Low + High_Low_Diff * Float_Random.Random(Env.Gen)
      );
      return Env.State;
   end Reset;
    
   function Step(Env : in out Environment_Type; action: Action_Type) return Step_Return_Type is
      -- Just apply the original step method as applying an action is deterministic
      Res : Step_Return_Type := Step(Env.State, action);
   begin
      -- Update the state before returning
      Env.State := Res.State;
      return Res;
   end Step;

    function Reset return Observation_Type is
        Low: constant Float := -0.05;
        High: constant Float := 0.05;
        High_Low_Diff: constant Float := High - Low;
        State: Observation_Type;
    begin
        -- TODO: Add an input argument to control RNG resets
        Float_Random.Reset(Gen);
        -- NOTE: We don't support an argument for specifying the range
        --       for the initial distribution of parameter values.
        -- TODO: Just return, no need for intermediate variable
        State := Observation_Type'(
            X => Threshold_Type(Low + High_Low_Diff * Float_Random.Random(Gen)),
            X_Dot => Low + High_Low_Diff * Float_Random.Random(Gen),
            Theta => Theta_Threshold_Type(Low + High_Low_Diff * Float_Random.Random(Gen)),
            Theta_Dot => Low + High_Low_Diff * Float_Random.Random(Gen)
        );
        return State;
    end Reset;
    
    function Step(State: Observation_Type; action: Action_Type) return Step_Return_Type is
        X: Threshold_Type := State.X;
        X_Dot: Float := State.X_Dot;
        Theta: Theta_Threshold_Type := State.Theta;
        Theta_Dot: Float := State.Theta_Dot;
        Force: Float := (if action = Right then Force_Mag else -Force_Mag);
        Costheta: Float := Cos(Float(Theta));
        Sintheta: Float := Sin(Float(Theta));
        Temp: Float;
        Thetaacc: Float;
        Xacc: Float;
        -- Return components
        Reward: Float;
        Output_State: Observation_Type;
        Terminated: Boolean;
        Return_Value: Step_Return_Type;
    begin
        Temp := (
            Force + Polemass_Length * (Theta_Dot ** 2) * Sintheta
        ) / Total_Mass;
        Thetaacc := (Gravity * Sintheta - Costheta * Temp) / (
            Length
            * (4.0 / 3.0 - Masspole * (costheta ** 2) / Total_Mass)
        );
        Xacc := Temp - Polemass_Length * Thetaacc * Costheta / Total_Mass;

        case Kinematics_Integrator is
            when Euler =>
                X := Threshold_Type(Float(X) + Tau * X_Dot);
                X_Dot := X_Dot + Tau * Xacc;
                Theta := Theta_Threshold_Type(Float(Theta) + Tau * Theta_Dot);
                Theta_Dot := Theta_Dot + Tau * Thetaacc;
            when Semi_Implicit =>
                X_Dot := X_Dot + Tau * Xacc;
                X := Threshold_Type(Float(X) + Tau * X_Dot);
                Theta_Dot := Theta_Dot + Tau * Thetaacc;
                Theta := Theta_Threshold_Type(Float(Theta) + Tau * Theta_Dot);
        end case;

        -- TODO: We need to update the environment (make it an out variable) and
        --       only return the observation
        Output_State := Observation_Type'(X => X, X_Dot => X_Dot, Theta => Theta, Theta_Dot => Theta_Dot);

        Terminated := (Float(X) < -X_Threshold)
            or (Float(X) > X_Threshold)
            or (Float(Theta) < -Theta_Threshold_Radians)
            or (Float(Theta) > Theta_Threshold_Radians);

        -- We stick to the logic of the Gymnasium implementation, but
        -- it might be clearer to have
        -- if not Sutton_Barto_Reward then 1.0
        -- else ...
        if not Terminated
        then 
            Reward := (if Sutton_Barto_Reward then 0.0 else 1.0);
        else
            Reward := (if Sutton_Barto_Reward then -1.0 else 1.0);
        end if;

        Return_Value := Step_Return_Type'(State => Output_State, Reward => Reward, Terminated => Terminated, Done => False);
        return Return_Value;
    end Step;
end RL.Envs.Cartpole;

