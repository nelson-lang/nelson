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
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
l = __line__('Parent', ax, 'XData', 1:3, 'YData', 1:3);
ax.HandleVisibility = 'off';
%=============================================================================
h = findobj(f, 'Type', 'axes');
asserts.istrue(isempty(h));
h = findobj(f, 'Type', 'line');
asserts.istrue(isempty(h));
%=============================================================================
h = findall(f, 'Type', 'axes');
asserts.istrue(any(h == ax));
h = findall(f, 'Type', 'line');
asserts.istrue(any(h == l));
%=============================================================================
h = allchild(f);
asserts.istrue(any(h == ax));
asserts.isfalse(any(h == l));
%=============================================================================
ax.HandleVisibility = 'on';
f.Position = [-2000 -2000 100 100];
asserts.istrue(isempty(findfigs()));
f.Visible = 'on';
h = findfigs();
asserts.istrue(any(h == f));
f.Visible = 'off';
%=============================================================================
close all
%=============================================================================
