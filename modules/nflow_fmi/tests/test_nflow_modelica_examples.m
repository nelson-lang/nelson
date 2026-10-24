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
% RC: first-order charging, exact vC(t) = 1 - e^{-t} (R = C = V = 1).
[t, vC] = nflow_modelica_example_run([exdir, '/RC.mo'], 'RC', 6.0, 0.02, 'vC');
assert_istrue(abs(vC(1) - 0.0) < 1e-9);
assert_istrue(abs(vC(end) - (1 - exp(-6))) < 5e-3);
% mid-trajectory point against the analytical curve
[~, k1] = min(abs(t - 1.0));
assert_istrue(abs(vC(k1) - (1 - exp(-1))) < 2e-2);
%=============================================================================
