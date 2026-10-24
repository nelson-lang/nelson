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
% Creating many objects in a loop must stay roughly linear. The per-attach full
% axes recompute (a rescan of every sibling) made this O(N^2): ~1000 patches
% used to take tens of seconds. With the deferred recompute it is a fraction of
% a second, so a generous wall-clock bound catches an O(N^2) regression without
% being flaky.
%=============================================================================
reset(groot());
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
hold(ax, 'on');
N = 1000;
x = [0; 0.17; 0.17; 0];
y = ones(4, 1);
z = [0; 0; 0.17; 0.17];
tic();
for k = 1:N
  patch(x, y, z, 'y');
end
elapsed = toc();
asserts.istrue(elapsed < 15);
%=============================================================================
% All children are present and the limits recompute once, correctly.
asserts.isequal(numel(get(ax, 'Children')), N);
drawnow();
asserts.isequal(numel(get(ax, 'Children')), N);
close(f);
%=============================================================================
