%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Parity with the reference: the first pixel of the screen is at 1, so a figure Position in
% pixels has x and y shifted by 1 against the other units (which start at 0).
%=============================================================================
screen = get(0, 'ScreenSize');
assert_istrue(close_to(screen(1:2), [1 1]));
scale = [screen(3) screen(4) screen(3) screen(4)];
f = figure('Visible', 'off', 'Position', [100 100 600 400]);
assert_istrue(close_to(f.Position, [100 100 600 400]));
assert_istrue(close_to(f.InnerPosition, [100 100 600 400]));
f.Units = 'normalized';
assert_istrue(close_to(f.Position .* scale, [99 99 600 400]));
f.Units = 'points';
assert_istrue(close_to(f.Position, [99 99 600 400] * 0.75));
f.Units = 'inches';
assert_istrue(close_to(f.Position, [99 99 600 400] / 96));
f.Units = 'pixels';
assert_istrue(close_to(f.Position, [100 100 600 400]));
%=============================================================================
f.Position = [1 1 600 400];
f.Units = 'normalized';
assert_istrue(close_to(f.Position(1:2), [0 0]));
f.Units = 'pixels';
assert_istrue(close_to(f.Position, [1 1 600 400]));
close(f);
%=============================================================================
function ok = close_to(actual, expected)
  ok = isequal(size(actual), size(expected)) && all(abs(actual - expected) < 1e-6);
end
%=============================================================================
