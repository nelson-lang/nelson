%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
close all
f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [100 120 420 310]);
%=============================================================================
ax = axes('Parent', f);
drawnow();
assert_vec_close(ax.Position, ax.InnerPosition, 1e-9);
ax.Position = [0.2 0.2 0.6 0.5];
drawnow();
asserts.isequal(ax.PositionConstraint, 'innerposition');
assert_vec_close(ax.Position, ax.InnerPosition, 1e-9);
ax.InnerPosition = [0.25 0.25 0.55 0.45];
drawnow();
asserts.isequal(ax.PositionConstraint, 'innerposition');
assert_vec_close(ax.Position, [0.25 0.25 0.55 0.45], 1e-9);
ax.OuterPosition = [0.1 0.1 0.8 0.75];
drawnow();
asserts.isequal(ax.PositionConstraint, 'outerposition');
assert_vec_close(ax.OuterPosition, [0.1 0.1 0.8 0.75], 1e-9);
assert_vec_close(ax.Position, ax.InnerPosition, 1e-9);
%=============================================================================
function assert_vec_close(actual, expected, tolerance)
  asserts.isequal(size(actual), size(expected));
  asserts.istrue(all(abs(actual - expected) <= tolerance));
end
%=============================================================================
function assert_readonly(h, name)
  failed = false;
  try
    set(h, name, [1 2 3 4]);
  catch
    failed = true;
  end
  asserts.istrue(failed);
end
%=============================================================================
