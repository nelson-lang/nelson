%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~modelicaInfo().capable
  return
end
exdir = [modulepath('nflow_fmi'), '/examples/modelica'];
%=============================================================================
% Thermal: first-order cooling, exact T(t) = Tamb + (T0-Tamb) e^{-t/(Rth Cth)}.
[t, T] = nflow_modelica_example_run([exdir, '/Thermal.mo'], 'Thermal', 30.0, 0.1, 'T');
assert_istrue(abs(T(1) - 100.0) < 1e-9);
assert_istrue(abs(T(end) - (20.0 + 80.0 * exp(-30.0 / 6.0))) < 5e-2); % RthCth = 6
%=============================================================================
%=============================================================================
