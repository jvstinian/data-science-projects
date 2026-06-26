with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Cartpole is
    function Get_Sutton_Barto_Reward return Boolean is
    begin
        return Sutton_Barto_Reward;
    end Get_Sutton_Barto_Reward;
    
    procedure Set_Sutton_Barto_Reward(Use_Sutton_Barto_Reward: Boolean) is
    begin
        Sutton_Barto_Reward := Use_Sutton_Barto_Reward;
    end Set_Sutton_Barto_Reward;
--     function Gcd(A, B: Integer) return Integer is
--         R: Integer;
--     begin
-- 	R := A rem B;
-- 	if R = 0
-- 	then
-- 	    return B;
-- 	else
-- 	    return Gcd(B, R);
-- 	end if;
--     end Gcd;
-- 
--     A : Integer := 221;
--     B : Integer := 26;
--     R : Integer;
-- begin
--     R := Gcd(A, B);
--     Put("GCD" & A'Image & ", " & B'Image & ") = ");
--     Put(R'Image);
--     New_Line;

    function Reset return Observation_Type is
        -- The following aren't currently used
        -- seed: int | None = None,
        -- options: dict | None = None,
        Low: constant Float := -0.05;
        High: constant Float := 0.05;
        High_Low_Diff: constant Float := High - Low;
        State: Observation_Type;
    begin
        -- super().reset(seed=seed)
        Float_Random.Reset(Gen);
        -- # Note that if you use custom reset bounds, it may lead to out-of-bound
        -- # state/observations.
        -- NOTE (JS): I haven't looked at the details of the implementation of the following method, 
        --            I am just guessing here.
        -- low, high = utils.maybe_parse_reset_bounds(
        --       options,
        --       -0.05,
        --       0.05,  # default low
        -- )  # default high

        -- self.state = self.np_random.uniform(low=low, high=high, size=(4,))
        State := Observation_Type'(
            X => Threshold_Type(Low + High_Low_Diff * Float_Random.Random(Gen)),
            X_Dot => Low + High_Low_Diff * Float_Random.Random(Gen),
            Theta => Theta_Threshold_Type(Low + High_Low_Diff * Float_Random.Random(Gen)),
            Theta_Dot => Low + High_Low_Diff * Float_Random.Random(Gen)
        );
        -- self.steps_beyond_terminated = None
        -- if self.render_mode == "human":
        --     self.render()
        -- return np.array(self.state, dtype=np.float32), {}
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
        Done: Boolean;
        Return_Value: Step_Return_Type;
    begin
        -- # For the interested reader:
        -- # https://coneural.org/florian/papers/05_cart_pole.pdf
        Temp := (
            Force + Polemass_Length * (Theta_Dot ** 2) * Sintheta
        ) / Total_Mass;
        Thetaacc := (Gravity * Sintheta - Costheta * Temp) / (
            Length
            * (4.0 / 3.0 - Masspole * (costheta ** 2) / Total_Mass)
        );
        Xacc := Temp - Polemass_Length * Thetaacc * Costheta / Total_Mass;

        -- if self.kinematics_integrator == "euler":
        X := Threshold_Type(Float(X) + Tau * X_Dot);
        X_Dot := X_Dot + Tau * Xacc;
        Theta := Theta_Threshold_Type(Float(Theta) + Tau * Theta_Dot);
        Theta_Dot := Theta_Dot + Tau * Thetaacc;
        -- else:  # semi-implicit euler
        --     X_Dot = X_Dot + self.Tau * Xacc
        --        x = x + self.Tau * X_Dot
        --        Theta_Dot = Theta_Dot + self.Tau * thetaacc
        --        Theta = theta + self.Tau * Theta_Dot
        --
        Output_State := Observation_Type'(X => X, X_Dot => X_Dot, Theta => Theta, Theta_Dot => Theta_Dot);

        Terminated := (Float(X) < -Float(X_Threshold))
            or (Float(X) > Float(X_Threshold))
            or (Float(Theta) < -Float(Theta_Threshold_Radians))
            or (Float(Theta) > Float(Theta_Threshold_Radians));

        if not Terminated
        then 
            Reward := (if Sutton_Barto_Reward then 0.0 else 1.0);
        else
            Reward := (if Sutton_Barto_Reward then -1.0 else 1.0);
        end if;
        -- else if steps_beyond_terminated is None:
        --     # Pole just fell!
        --     self.steps_beyond_terminated = 0

        --     reward = -1.0 if self._sutton_barto_reward else 1.0
        -- else:
        --     if self.steps_beyond_terminated == 0:
        --         logger.warn(
        --             "You are calling 'step()' even though this environment has already returned terminated = True. "
        --             "You should always call 'reset()' once you receive 'terminated = True' -- any further steps are undefined behavior."
        --         )
        --     self.steps_beyond_terminated += 1

        --     reward = -1.0 if self._sutton_barto_reward else 0.0

        -- if self.render_mode == "human":
        --     self.render()

        -- # truncation=False as the time limit is handled by the `TimeLimit` wrapper added during `make`
        -- return np.array(self.state, dtype=np.float32), reward, terminated, False, {}
        Return_Value := Step_Return_Type'(State => Output_State, Reward => Reward, Terminated => Terminated, Done => False);
        return Return_Value;
    end Step;
end Cartpole;

