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
f = figure('Visible', 'off');
ax = axes('Parent', f);
set(ax, 'GridAlphaMode', 'auto', 'GridColorMode', 'auto', 'XGrid', 'on');
ax.GridAlpha = 0.5;
ax.GridColor = [0.2 0.3 0.4];
asserts.isequal(ax.GridAlphaMode, 'manual');
asserts.isequal(ax.GridColorMode, 'manual');
close(f);
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
