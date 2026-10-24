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
wc = wordcloud({'alpha', 'beta'}, [5 3]);
wc.Position = [0.2 0.2 0.6 0.6];
drawnow();
asserts.isequal(wc.PositionConstraint, 'innerposition');
assert_vec_close(wc.Position, wc.InnerPosition, 1e-9);
assert_vec_close(wc.Position, wc.OuterPosition, 1e-9);
wc.OuterPosition = [0.1 0.1 0.8 0.8];
drawnow();
asserts.isequal(wc.PositionConstraint, 'outerposition');
assert_vec_close(wc.Position, wc.InnerPosition, 1e-9);
assert_vec_close(wc.Position, wc.OuterPosition, 1e-9);
wc.Units = 'pixels';
drawnow();
assert_vec_close(wc.Position, wc.InnerPosition, 1e-9);
assert_vec_close(wc.Position, wc.OuterPosition, 1e-9);
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
