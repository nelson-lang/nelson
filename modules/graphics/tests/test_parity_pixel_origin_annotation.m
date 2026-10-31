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
% Parity with the reference: the pixel Position of an annotation has x and y shifted by 1
% against the other units, as for the axes and the ui components.
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 600 400]);
drawnow();
scale = [600 400 600 400];
%=============================================================================
an = annotation(f, 'textbox', [0.2 0.2 0.3 0.3]);
an.Units = 'pixels';
assert_istrue(close_to(an.Position, [121 81 180 120]));
an.Position = [100 100 50 50];
an.Units = 'normalized';
assert_istrue(close_to(an.Position .* scale, [99 99 50 50]));
%=============================================================================
an2 = annotation(f, 'rectangle', 'Units', 'pixels', 'Position', [10 10 50 50]);
an2.Units = 'normalized';
assert_istrue(close_to(an2.Position .* scale, [9 9 50 50]));
an2.Units = 'pixels';
assert_istrue(close_to(an2.Position, [10 10 50 50]));
close(f);
%=============================================================================
function ok = close_to(actual, expected)
  ok = isequal(size(actual), size(expected)) && all(abs(actual - expected) < 1e-6);
end
%=============================================================================
