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
% Attaching a child defers the axes recompute (for speed), so this guards that
% the auto limits still reflect every child WITHOUT an explicit drawnow in
% between, and that an explicit draw afterwards does not change them.
%=============================================================================
reset(groot());
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
hold(ax, 'on');
N = 50;
for k = 1:N
  x = [0; 0.17; 0.17; 0] + k;
  y = ones(4, 1);
  z = [0; 0; 0.17; 0.17] + k * 0.1;
  patch(x, y, z, 'y');
end
%=============================================================================
% No drawnow: the limits must already span all children.
xl = get(ax, 'XLim');
yl = get(ax, 'YLim');
zl = get(ax, 'ZLim');
asserts.istrue(xl(1) <= 1);
asserts.istrue(xl(2) >= N);
asserts.istrue(yl(1) <= 1);
asserts.istrue(yl(2) >= 1);
asserts.istrue(zl(2) >= N * 0.1);
%=============================================================================
% The DataLimits track the raw child extents and must be fresh too.
dl = get(ax, 'DataLimits');
asserts.istrue(dl(1) <= 1);
asserts.istrue(dl(2) >= N);
%=============================================================================
% The dot-indexing read path flushes the same deferred state.
asserts.isequal(ax.XLim, xl);
asserts.isequal(ax.YLim, yl);
%=============================================================================
% An explicit draw must not change the already-computed limits.
drawnow();
asserts.isequal(get(ax, 'XLim'), xl);
asserts.isequal(get(ax, 'YLim'), yl);
asserts.isequal(get(ax, 'ZLim'), zl);
asserts.isequal(numel(get(ax, 'Children')), N);
close(f);
%=============================================================================
% Reading mid-loop must reflect exactly the children created so far: the
% deferred recompute flushes on read, it does not lag behind.
f = figure('Visible', 'off');
ax = axes('Parent', f);
hold(ax, 'on');
patch([0; 1; 1; 0], [0; 0; 1; 1], 'y');
xlFirst = get(ax, 'XLim');
patch([0; 10; 10; 0], [0; 0; 1; 1], 'y');
xlSecond = get(ax, 'XLim');
asserts.istrue(xlSecond(2) >= 10);
asserts.istrue(xlSecond(2) > xlFirst(2));
close(f);
%=============================================================================
