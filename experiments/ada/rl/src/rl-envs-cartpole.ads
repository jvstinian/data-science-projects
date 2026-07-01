with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Float_Random;

-- This package provides an implementation of the cart-pole environment described
-- by Barto, Sutton, and Anderson in
-- ["Neuronlike Adaptive Elements That Can Solve Difficult Learning Control Problem"](https://ieeexplore.ieee.org/document/6313077)
--
-- This implementation is a translation from the Python Gymnasium package
-- https://github.com/Farama-Foundation/Gymnasium/blob/169a247ab478b9f7e36edb96d06574ae60abb03c/gymnasium/envs/classic_control/cartpole.py
-- Some differences from the Gymnasium implementation are as follows:
-- * we do not include an options parameter for the reset method, which in 
--   Gymnasium allows the bounds used to determine the new random state to
--   be changed, 
-- * we omit the steps_beyond_terminated variable
package RL.Envs.Cartpole is
    package Float_Random renames Ada.Numerics.Float_Random;

    -- Position and angle thresholds at which to fail the episode.
    X_Threshold: constant Float := 2.4;
    Theta_Threshold_Radians: constant Float := 12.0 * 2.0 * Ada.Numerics.Pi / 360.0;

    X_Threshold_Upper: constant Float := 2.0 * X_Threshold;
    X_Threshold_Lower : constant Float := -2.0 * X_Threshold;
    Theta_Threshold_Upper: constant Float := 2.0 * Theta_Threshold_Radians;
    Theta_Threshold_Lower : constant Float := -2.0 * Theta_Threshold_Radians;

    type Action_Type is (Left, Right);

    -- Position and angle are allowed to assume values up to twice the
    -- corresponding failure threshold.
    -- This allows the failing observation to still be within bounds.
    -- This is particularly important in Ada so that a value constraint
    -- error is not raised before we can specify episode termination.
    type Threshold_Type is new Float range X_Threshold_Lower .. X_Threshold_Upper;
    type Theta_Threshold_Type is new Float range Theta_Threshold_Lower .. Theta_Threshold_Upper;

    -- Configguration
    --
    -- Sutton_Barto_Reward determines the reward values.
    -- When True, a reward of 0 is awarded for non-terminating steps and -1 for the terminating step
    -- When False (the default), 1 is awarded for each step.
    -- 
    -- Kinematics_Integrator_Type determines how the position and angle are updated.
    type Kinematics_Integrator_Type is (Euler, Semi_Implicit);

    type Config_Type is record
       Sutton_Barto_Reward: Boolean := False;
       Kinematics_Integrator: Kinematics_Integrator_Type := Euler;
    end record;

    type Environment_Type is limited private;
    
    type Observation_Type is record
        X: Threshold_Type;
        X_Dot: Float;
        Theta: Theta_Threshold_Type;
        Theta_Dot: Float;
    end record;
    
    type Step_Return_Type is record
        Observation: Observation_Type;
        Reward: Float;
        Terminated: Boolean;
    end record;
   
    -- RL Methods
    function Make(Config: Config_Type) return Environment_Type;
    function Reset(Env : in out Environment_Type; Seed_Reset : Seed_Reset_Type) return Observation_Type;
    function Step(Env : in out Environment_Type; action: Action_Type) return Step_Return_Type;

private
    -- Constants used by the environment
    Gravity: constant Float := 9.8;
    Masscart: constant Float := 1.0;
    Masspole: constant Float := 0.1;
    Total_Mass: constant Float := Masspole + Masscart;
    Length: constant Float := 0.5;  -- half the pole's length
    Polemass_Length: constant Float := Masspole * Length;
    Force_Mag: constant Float := 10.0;
    Tau: constant Float := 0.02; -- seconds between state updates

    type Environment_Type is limited record
       Config : Config_Type;
       Gen : Float_Random.Generator;
       State : Observation_Type;
    end record;

    function Apply_Action (Config: Config_Type; State: Observation_Type; Action: Action_Type) return Step_Return_Type;
end RL.Envs.Cartpole;
