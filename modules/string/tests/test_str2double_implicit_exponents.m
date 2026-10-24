%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
invalid = {'1+2', '1-2', '-1+2', '+1-2', '1.5+03', '.25-2', ...
  '12+003', '1 + 2', '1 - 2', '1e2+3', '1d2-3', '2^3', 'sin(1)', ...
  '1D+2', '1d-2', '+1D-2', '-1d-2-3D+2j'};
for k = 1:numel(invalid)
  asserts.isequal(str2double(invalid{k}), NaN);
end
asserts.isequal(str2double(invalid), NaN(size(invalid)));
asserts.isequal(str2double(string(invalid)), NaN(size(invalid)));
valid = {'1e+2', '1e-2', '-1e+2', ...
  '1e400', '-1e400', '1e-400', '0x1A', '1,200', '1+2i', '1-2j', ...
  '1e+2+3e-2i'};
expected = [100, .01, -100, Inf, -Inf, 0, 26, 1200, ...
  1 + 2i, 1 - 2i, 100 + .03i];
asserts.isequal(str2double(valid), expected);
asserts.isequal(str2double(string(valid)), expected);
