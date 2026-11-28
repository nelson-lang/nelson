%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Chart Position uses pixel origin 1 (pixel units start at 1) and converts between units.
%=============================================================================
f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [100 100 560 420]);
h = heatmap(magic(4));
assert_isequal(char(h.Units), 'normalized');
% the plot box leaves the room of the colorbar on its right
normalizedPosition = h.Position;
assert_istrue(close_to(normalizedPosition([2 4]), [0.11 0.815]));
assert_istrue(abs(normalizedPosition(1) - 0.13) < 0.02 && normalizedPosition(3) < 0.775);
h.Units = 'pixels';
drawnow();
assert_istrue(close_to(h.Position, [1 1 0 0] + normalizedPosition .* [560 420 560 420]));
h.Units = 'normalized';
drawnow();
assert_istrue(close_to(h.Position, normalizedPosition));
%=============================================================================
% a pixel Position reads back unchanged and converts with origin 1
h.Units = 'pixels';
drawnow();
h.Position = [50 60 300 200];
drawnow();
assert_istrue(close_to(h.Position, [50 60 300 200]));
h.Units = 'normalized';
drawnow();
assert_istrue(close_to(h.Position, [49 59 300 200] ./ [560 420 560 420]));
close(f);
%=============================================================================
% wordcloud default Position [0 0 1 1] -> pixels [1 1 W H]
f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [100 100 560 420]);
w = wordcloud(["alpha" "beta" "gamma"], [3 2 1]);
assert_istrue(close_to(w.Position, [0 0 1 1]));
w.Units = 'pixels';
drawnow();
assert_istrue(close_to(w.Position, [1 1 560 420]));
w.Units = 'normalized';
drawnow();
assert_istrue(close_to(w.Position, [0 0 1 1]));
close(f);
%=============================================================================
function ok = close_to(actual, expected)
  ok = isequal(size(actual), size(expected)) && all(abs(actual - expected) < 1e-6);
end
%=============================================================================
