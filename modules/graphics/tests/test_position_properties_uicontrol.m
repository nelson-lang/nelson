%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
close all
f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [100 120 420 310]);
%=============================================================================
u = uicontrol('Parent', f, 'Style', 'text', 'Position', [30 40 120 30]);
drawnow();
assert_vec_close(u.Position, u.InnerPosition, 1e-9);
assert_vec_close(u.Position, u.OuterPosition, 1e-9);
u.InnerPosition = [35 45 130 35];
drawnow();
assert_vec_close(u.Position, [35 45 130 35], 1e-9);
assert_vec_close(u.Position, u.InnerPosition, 1e-9);
assert_vec_close(u.Position, u.OuterPosition, 1e-9);
u.OuterPosition = [25 35 140 45];
drawnow();
assert_vec_close(u.Position, [25 35 140 45], 1e-9);
assert_vec_close(u.Position, u.InnerPosition, 1e-9);
assert_vec_close(u.Position, u.OuterPosition, 1e-9);
u.Units = 'normalized';
drawnow();
assert_vec_close(u.Position, u.InnerPosition, 1e-9);
assert_vec_close(u.Position, u.OuterPosition, 1e-9);
u.Units = 'pixels';
drawnow();
assert_vec_close(u.Position, [25 35 140 45], 1);
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
