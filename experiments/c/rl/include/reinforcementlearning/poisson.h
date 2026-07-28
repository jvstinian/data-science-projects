/*
---------------------------------------------------------------------------
--  GENERIC RANDOM FUNCTIONS
--
--  Functions facilitating computations with various standard random distributions.
--  These functions can be called by more elaborate object-oriented
--  random distribution classes with CDF, Inverse_CDF, or Simulate methods.
--  The purpose here is a direct, "ad-hoc" utility.
---------------------------------------------------------------------------
--
--  This is part of the Mathpaqs collection of mathematical packages.
--  Latest version may be available at:
--      home page:     http://mathpaqs.sf.net/
--      project page:  http://sf.net/projects/mathpaqs/
--      mirror:        https://github.com/zertovitch/mathpaqs
--
-------------------------
--  Legal licensing note:

--  Copyright (c) 2010 .. 2023 Gautier de Montmollin
--  Parts originally created by Stephen L. Moshier (see implementation for details)

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 9-Feb-2011 on the site
--  http://www.opensource.org/licenses/mit-license.php
*/

/*
-----------------------------------------
--  1.2 Discrete random distributions  --
-----------------------------------------

----------------------------
--  Poisson distribution  --
----------------------------

-- lambda is the frequency. E(N) = lambda when N ~ Poisson(lambda)
-- U is meant to be an U(0,1) uniform generator
-- NB: This method is suboptimal, since the random generator
-- is called one or more times. Ideally U should be called once and passed
-- as a Real parameter to the Poisson function.
--
*/
unsigned int poisson(float lambda);

