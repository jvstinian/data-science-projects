with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Float_Random; -- use Ada.Numerics.Float_Random;

package Cartpole is
    package Float_Random renames Ada.Numerics.Float_Random;

    X_Threshold: constant Float := 2.4;
    Theta_Threshold_Radians: constant Float := 12.0 * 2.0 * Ada.Numerics.Pi / 360.0;

    X_Threshold_Upper: constant Float := 2.0 * X_Threshold;
    X_Threshold_Lower : constant Float := -2.0 * X_Threshold;
    Theta_Threshold_Upper: constant Float := 2.0 * Theta_Threshold_Radians;
    Theta_Threshold_Lower : constant Float := -2.0 * Theta_Threshold_Radians;

    -- render_mode: str | None = None

    -- self.action_space = spaces.Discrete(2)
    type Action_Type is (Left, Right);

    -- Angle at which to fail the episode
    -- Angle limit set to 2 * theta_threshold_radians so failing observation
    -- is still within bounds.
    type Threshold_Type is new Float range X_Threshold_Lower .. X_Threshold_Upper;
    type Theta_Threshold_Type is new Float range Theta_Threshold_Lower .. Theta_Threshold_Upper;

    -- self.observation_space = spaces.Box(-high, high, dtype=np.float32)
    type Observation_Type is record
        X: Threshold_Type;
        X_Dot: Float;
        Theta: Theta_Threshold_Type;
        Theta_Dot: Float;
    end record;
    
    type Step_Return_Type is record
        State: Observation_Type;
        Reward: Float;
        Terminated: Boolean;
        Done: Boolean;
    end record;
    
    function Get_Sutton_Barto_Reward return Boolean;
    procedure Set_Sutton_Barto_Reward(Use_Sutton_Barto_Reward: Boolean);
    function Reset return Observation_Type;
    function Step(state: Observation_Type; action: Action_Type) return Step_Return_Type;

private
    Sutton_Barto_Reward: Boolean := False;
    Gravity: constant Float := 9.8;
    Masscart: constant Float := 1.0;
    Masspole: constant Float := 0.1;
    Total_Mass: constant Float := Masspole + Masscart;
    Length: constant Float := 0.5;  -- actually half the pole's length
    Polemass_Length: constant Float := Masspole * Length;
    Force_Mag: constant Float := 10.0;
    Tau: constant Float := 0.02; -- seconds between state updates
    -- self.kinematics_integrator = "euler"
    --
    -- self.steps_beyond_terminated = None
    Gen: Float_Random.Generator;

--         high = np.array(
--             [
--                 self.x_threshold * 2,
--                 np.inf,
--                 self.theta_threshold_radians * 2,
--                 np.inf,
--             ],
--             dtype=np.float32,
--         )
-- 
--         self.render_mode = render_mode
--         self.screen_width = 600
--         self.screen_height = 400
--         self.screen = None
--         self.clock = None
--         self.isopen = True
--         self.state: np.ndarray | None = None
-- 
end Cartpole;

