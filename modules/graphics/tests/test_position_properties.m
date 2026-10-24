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
close all
%=============================================================================
f = figure('Visible', 'off', 'Units', 'pixels', 'Position', [100 120 420 310]);
assert_vec_close(f.Position, f.InnerPosition, 0);
f.InnerPosition = [110 130 430 320];
drawnow();
assert_vec_close(f.Position, [110 130 430 320], 1e-9);
assert_vec_close(f.InnerPosition, f.Position, 1e-9);
f.OuterPosition = [120 140 440 330];
drawnow();
assert_vec_close(f.Position, f.InnerPosition, 1e-9);
assert_vec_close(f.OuterPosition, [120 140 440 330], 3);
f.Position = [130 150 450 340];
drawnow();
assert_vec_close(f.InnerPosition, [130 150 450 340], 1e-9);
f.Units = 'inches';
drawnow();
assert_vec_close(f.Position, [130 150 450 340] / 96, 1e-9);
assert_vec_close(f.Position, f.InnerPosition, 1e-9);
assert_vec_close(f.Position, f.OuterPosition, 1e-9);
f.Units = 'pixels';
drawnow();
assert_vec_close(f.Position, [130 150 450 340], 1e-9);
assert_vec_close(f.Position, f.InnerPosition, 1e-9);
assert_vec_close(f.Position, f.OuterPosition, 1e-9);
%=============================================================================
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
