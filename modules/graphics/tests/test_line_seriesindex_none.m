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
% Line and stair SeriesIndex accepts a nonnegative integer or 'none' (kept as text).
%=============================================================================
f = figure('Visible', 'off');
ax = axes(f);
l = plot(ax, 1:3);
assert_isequal(l.SeriesIndex, 1);
l.SeriesIndex = 'none';
assert_isequal(l.SeriesIndex, 'none');
l.SeriesIndex = 3;
assert_isequal(l.SeriesIndex, 3);
l.SeriesIndex = 0;
assert_isequal(l.SeriesIndex, 0);
assert_istrue(setFails(l, 'SeriesIndex', -1));
assert_istrue(setFails(l, 'SeriesIndex', 1.5));
assert_istrue(setFails(l, 'SeriesIndex', 'zzz'));
s = stairs(ax, 1:3);
s.SeriesIndex = 'none';
assert_isequal(s.SeriesIndex, 'none');
delete(f);
%=============================================================================
function failed = setFails(h, name, value)
  failed = false;
  try
    set(h, name, value);
  catch
    failed = true;
  end
end
%=============================================================================
