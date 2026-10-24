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
h = findobj();
asserts.isfalse(isempty(h));
asserts.istrue(isgraphics(h(1)));
asserts.istrue(any(h == groot()));
%=============================================================================
asserts.istrue(isempty(findobj(0, 'Tag', 'ti92App')));
asserts.istrue(isempty(findobj(groot(), 'Tag', 'ti92App')));
%=============================================================================
f = figure();
ax = axes('Parent', f);
l1 = line(1:10, 1:10, 'Parent', ax, 'Tag', 'linear', 'Color', [1 0 0]);
l2 = line(1:10, (1:10) .^ 2, 'Parent', ax, 'Tag', 'quadratic');
l3 = line(1:10, exp(1:10), 'Parent', ax, 'Tag', 'exponential');
%=============================================================================
h = findobj('Type', 'line');
asserts.isequal(numel(h), 3);
asserts.istrue(all(isgraphics(h, 'line')));
%=============================================================================
h = findobj(gca(), 'Type', 'line');
asserts.isequal(numel(h), 3);
asserts.istrue(any(h == l1));
asserts.istrue(any(h == l2));
asserts.istrue(any(h == l3));
%=============================================================================
h = findobj(gcf());
asserts.istrue(any(h == f));
asserts.istrue(any(h == ax));
asserts.istrue(any(h == l1));
%=============================================================================
h = findobj(f, 'flat', 'Type', 'figure');
asserts.isequal(h, f);
h = findobj(f, 'flat', 'Type', 'axes');
asserts.istrue(isempty(h));
h = findobj(f, '-depth', 1, 'Type', 'axes');
asserts.isequal(h, ax);
h = findobj(f, '-depth', 1, 'Type', 'line');
asserts.istrue(isempty(h));
h = findobj(f, '-depth', 2, 'Type', 'line');
asserts.isequal(numel(h), 3);
h = findobj(f, '-depth', inf, 'Type', 'line');
asserts.isequal(numel(h), 3);
%=============================================================================
h = findobj('Tag', 'linear');
asserts.isequal(h, l1);
h = findobj(0, 'Tag', 'linear');
asserts.isequal(h, l1);
h = findobj(groot(), 'Tag', 'linear');
asserts.isequal(h, l1);
h = findobj('-regexp', 'Tag', 'quad');
asserts.isequal(h, l2);
h = findobj('-property', 'Marker');
asserts.istrue(any(h == l1));
asserts.istrue(any(h == l2));
asserts.istrue(any(h == l3));
%=============================================================================
h = findobj('Type', 'line', '-and', '-not', {'Tag', 'linear', '-or', 'Tag', 'quadratic'});
asserts.isequal(h, l3);
h = findobj({'Tag', 'linear', '-or', 'Tag', 'quadratic'}, '-and', {'Type', 'line'});
asserts.isequal(numel(h), 2);
asserts.istrue(any(h == l1));
asserts.istrue(any(h == l2));
h = findobj('Type', 'line', '-and', {'Tag', 'linear', '-xor', 'Tag', 'quadratic'});
asserts.isequal(numel(h), 2);
%=============================================================================
h = findobj('Color', 'red');
asserts.istrue(any(h == l1));
h = findobj('Color', 'r');
asserts.istrue(any(h == l1));
%=============================================================================
ax.HandleVisibility = 'off';
h = findobj(f, 'Type', 'axes');
asserts.istrue(isempty(h));
h = findobj(f, 'Type', 'line');
asserts.istrue(isempty(h));
h = findobj(0, 'Tag', 'linear');
asserts.istrue(isempty(h));
h = findall(0, 'Tag', 'linear');
asserts.isequal(h, l1);
ax.HandleVisibility = 'on';
%=============================================================================
h = findobj([f; ax], 'Type', 'line');
asserts.isequal(sum(h == l1), 2);
%=============================================================================
asserts.checkerror('findobj(''Type'')', getString(message('Nelson:findobj:invalidExpression')));
asserts.checkerror('findobj(gcf(), ''-depth'', -1)', getString(message('Nelson:findobj:invalidDepth')));
%=============================================================================
close all
%=============================================================================
