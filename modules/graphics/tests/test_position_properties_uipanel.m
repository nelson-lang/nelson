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
uf = uifigure('Visible', 'off', 'Position', [100 120 420 310]);
%=============================================================================
p = uipanel(uf, 'Position', [30 40 200 120]);
drawnow();
assert_vec_close(p.Position, p.OuterPosition, 1e-9);
assert_readonly(p, 'InnerPosition');
p.OuterPosition = [25 35 210 130];
drawnow();
assert_vec_close(p.Position, [25 35 210 130], 1e-9);
assert_vec_close(p.OuterPosition, p.Position, 1e-9);
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
