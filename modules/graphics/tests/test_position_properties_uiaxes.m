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
%=============================================================================
uf = uifigure('Visible', 'off', 'Position', [100 120 420 310]);
uax = uiaxes(uf);
drawnow();
asserts.isequal(uax.PositionConstraint, 'outerposition');
assert_vec_close(uax.Position, uax.OuterPosition, 1e-9);
uax.Position = [80 70 360 260];
drawnow();
asserts.isequal(uax.PositionConstraint, 'outerposition');
assert_vec_close(uax.Position, [80 70 360 260], 1e-9);
assert_vec_close(uax.Position, uax.OuterPosition, 1e-9);
uax.InnerPosition = [90 80 340 240];
drawnow();
asserts.isequal(uax.PositionConstraint, 'innerposition');
assert_vec_close(uax.InnerPosition, [90 80 340 240], 1e-9);
assert_vec_close(uax.Position, uax.OuterPosition, 1e-9);
uax.OuterPosition = [70 60 380 280];
drawnow();
asserts.isequal(uax.PositionConstraint, 'outerposition');
assert_vec_close(uax.Position, uax.OuterPosition, 1e-9);
assert_vec_close(uax.OuterPosition, [70 60 380 280], 1e-9);
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
