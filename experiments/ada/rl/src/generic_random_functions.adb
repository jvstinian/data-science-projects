with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

package body Generic_Random_Functions is

  -- Ada 95 Quality and Style Guide, 7.2.7:
  -- Tests for
  --
  -- (1) absolute "equality" to 0 in storage,
  -- (2) absolute "equality" to 0 in computation,
  -- (3) relative "equality" to 0 in storage, and
  -- (4) relative "equality" to 0 in computation:
  --
  --  abs X <= Float_Type'Model_Small                      -- (1)
  --  abs X <= Float_Type'Base'Model_Small                 -- (2)
  --  abs X <= abs X * Float_Type'Model_Epsilon            -- (3)
  --  abs X <= abs X * Float_Type'Base'Model_Epsilon       -- (4)

  package GEF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  use GEF;

  function Almost_zero(X: Real) return Boolean is
  begin
    return  abs X <= Real'Base'Model_Small;
  end Almost_zero;

  function Poisson(lambda: Real) return Natural is
    lower_limit, product: Real;
    k: Integer;
  begin
    -- Algo found in:
    -- http://en.wikipedia.org/wiki/Poisson_distribution
    --   #Generating_Poisson-distributed_random_variables
    -- referring to:
    -- Knuth (whom else ?!), The Art of Computer Programming,
    -- Volume 2, Seminumerical algorithms, 3.4.1. Numerical Distributions,
    -- F. Important integer-valued distributions.
    lower_limit:= Exp(-lambda);
    k:= -1;
    product:= 1.0;
    loop
      k:= k + 1;
      product:= product * U;
      exit when product <= lower_limit;
    end loop;
    return k;
  end Poisson;

  function Pareto_inverse_CDF(q, threshold, minus_inv_alpha: Real) return Real is
  begin
    if Almost_zero(q) then
      return Real'Last;
    end if;
    return threshold * q ** (minus_inv_alpha);
  end Pareto_inverse_CDF;

  function Pareto_CDF(x, threshold, alpha: Real) return Real is
  begin
    return 1.0 - (threshold / x) ** alpha;
  end Pareto_CDF;

end Generic_Random_Functions;
