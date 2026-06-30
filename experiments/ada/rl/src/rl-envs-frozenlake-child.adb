with Ada.Text_IO; use Ada.Text_IO;

package body RL.Envs.Frozenlake.Child is
   procedure Dummy_Method is
   begin
       Put_Line("This is a dummy method in the child package.");
       Put_Line("Last value of discrete state: " & Alt_Discrete_State_Type'Last'Image);
   end Dummy_Method;
    
   -- Initialize Precise_DP_Model 
   function Get_Model(Config: Environment_Config) return Precise_Model_Type is
      DP_Model : Discrete_Model_Type := Get_Model(Config);
      Precise_DP_Model : Precise_Model_Type;
   begin
      for S0 in Precise_State_Type loop
         for A in Action_Type loop
            for S1 in Precise_State_Type loop
                Precise_DP_Model(S0, A, S1) := DP_Model(Discrete_State_Type(S0), A, Discrete_State_Type(S1));
            end loop;
         end loop;
      end loop;
      return Precise_DP_Model;
   end Get_Model;
end RL.Envs.Frozenlake.Child;
