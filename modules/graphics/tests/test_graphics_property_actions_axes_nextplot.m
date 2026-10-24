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
group1 = hggroup('Parent', ax);
group2 = hggroup('Parent', ax);
h = line([0 1], [0 1], 'Parent', group1);
asserts.istrue(any(group1.Children == h));
h.Parent = group2;
asserts.isfalse(any(group1.Children == h));
asserts.istrue(any(group2.Children == h));
h.HandleVisibility = 'off';
asserts.istrue(isempty(findobj(f, 'Type', 'line')));
h.HandleVisibility = 'on';
asserts.istrue(any(findobj(f, 'Type', 'line') == h));
close(f);
%=============================================================================
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
