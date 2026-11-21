%=============================================================================
% Copyright (c) 2024-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
model = [modulepath('nflow_blocks'), '/examples/causal/WGS84_Gravity_Annotated.nflow'];
r = jsondecode(__nflow_simulate__(fileread(model)));
assert_isequal(r.status, 'done');
g = r.signals.gravity;
if iscell(g), g = cell2mat(g); end
s = sin(0.6)^2;
expected = 9.7803253359 * (1 + 0.00193185265241 * s) / sqrt(1 - 0.00669437999013 * s);
assert_isapprox(g(1), expected, 1e-10);
h = 1000; a = 6378137; f = 1 / 298.257223563; m = 0.00344978650684;
expected_h = expected * (1 - 2 * (1 + f + m - 2 * f * s) * (h / a) + 3 * (h / a)^2);
gh = r.signals.gravity_height;
if iscell(gh), gh = cell2mat(gh); end
assert_isapprox(gh(1), expected_h, 1e-10);
%=============================================================================
