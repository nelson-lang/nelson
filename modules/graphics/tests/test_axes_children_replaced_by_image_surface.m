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
close all
reset(groot());
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
for k = 1:4
  set(f, 'CurrentAxes', ax);
  imagesc(k * magic(8));
  asserts.isequal(numel(ax.Children), 1);
end
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
[X, Y, Z] = peaks(16);
for k = 1:4
  set(f, 'CurrentAxes', ax);
  surf(X, Y, Z + k, 'EdgeColor', 'none');
  asserts.isequal(numel(ax.Children), 1);
end
close(f);
%=============================================================================
