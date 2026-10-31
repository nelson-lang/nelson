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
% Parity with the reference: an automatically placed legend or colorbar reads back in its Units
% (pixels start at 1, the other units at 0) after Units changed.
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 600 400]);
drawnow();
scale = [600 400 600 400];
ax = axes(f);
plot(ax, 1:3, 'DisplayName', 'a');
lg = legend(ax, 'a');
drawnow();
assert_isequal(lg.Units, 'normalized');
normalized = lg.Position;
lg.Units = 'pixels';
assert_istrue(close_to(lg.Position, normalized .* scale + [1 1 0 0]));
lg.Units = 'normalized';
assert_istrue(close_to(lg.Position, normalized));
lg.Units = 'pixels';
lg.Position = [100 100 80 40];
lg.Units = 'normalized';
assert_istrue(close_to(lg.Position .* scale, [99 99 80 40]));
%=============================================================================
cb = colorbar(ax);
drawnow();
cb.Units = 'pixels';
cb.Position = [500 50 20 200];
cb.Units = 'normalized';
assert_istrue(close_to(cb.Position .* scale, [499 49 20 200]));
close(f);
%=============================================================================
function ok = close_to(actual, expected)
  ok = isequal(size(actual), size(expected)) && all(abs(actual - expected) < 1e-6);
end
%=============================================================================
