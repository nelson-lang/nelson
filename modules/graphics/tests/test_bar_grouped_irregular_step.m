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
% Grouped bars at irregular positions: every bar of a series is shifted by
% the same offset, the group width times the smallest positive step of the
% whole position vector (here 0.5, between 4 and 4.5), not by a per-bar
% step. The step is computed once per series, so a 2000-bar series updates
% in a few milliseconds instead of sorting the positions once per bar.
%=============================================================================
close all
f = figure('Visible', 'off');
x = [1 2 4 4.5 8];
h = bar(x, [1 2 3 4 5; 5 4 3 2 1]');
assert_isequal(numel(h), 2);
groupWidth = min(0.8, 2 / 3.5);
shift = 0.5 * groupWidth / 2 * 0.5;
assert_isapprox(h(1).XEndPoints, x - shift, 1e-12);
assert_isapprox(h(2).XEndPoints, x + shift, 1e-12);
assert_isapprox(h(2).XEndPoints - h(1).XEndPoints, repmat(2 * shift, 1, 5), 1e-12);
%=============================================================================
% Labels follow the same per-series offset and a large series redraws.
set(h(1), 'Labels', {'a', 'b', 'c', 'd', 'e'});
drawnow();
n = 2000;
g = bar(1:n, rand(1, n));
for k = 1:3
  set(g, 'YData', rand(1, n));
  drawnow();
end
assert_isequal(numel(g.XEndPoints), n);
assert_isequal(g.XEndPoints, 1:n);
close(f);
%=============================================================================
