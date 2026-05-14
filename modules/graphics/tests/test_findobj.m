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
assert_isfalse(isempty(h));
assert_istrue(isgraphics(h(1)));
assert_istrue(any(h == groot()));
%=============================================================================
f = figure();
ax = axes('Parent', f);
l1 = line(1:10, 1:10, 'Parent', ax, 'Tag', 'linear', 'Color', [1 0 0]);
l2 = line(1:10, (1:10).^2, 'Parent', ax, 'Tag', 'quadratic');
l3 = line(1:10, exp(1:10), 'Parent', ax, 'Tag', 'exponential');
%=============================================================================
h = findobj('Type', 'line');
assert_isequal(numel(h), 3);
assert_istrue(all(isgraphics(h, 'line')));
%=============================================================================
h = findobj(gca(), 'Type', 'line');
assert_isequal(numel(h), 3);
assert_istrue(any(h == l1));
assert_istrue(any(h == l2));
assert_istrue(any(h == l3));
%=============================================================================
h = findobj(gcf());
assert_istrue(any(h == f));
assert_istrue(any(h == ax));
assert_istrue(any(h == l1));
%=============================================================================
h = findobj(f, 'flat', 'Type', 'figure');
assert_isequal(h, f);
h = findobj(f, 'flat', 'Type', 'axes');
assert_istrue(isempty(h));
h = findobj(f, '-depth', 1, 'Type', 'axes');
assert_isequal(h, ax);
h = findobj(f, '-depth', 1, 'Type', 'line');
assert_istrue(isempty(h));
h = findobj(f, '-depth', 2, 'Type', 'line');
assert_isequal(numel(h), 3);
h = findobj(f, '-depth', inf, 'Type', 'line');
assert_isequal(numel(h), 3);
%=============================================================================
h = findobj('Tag', 'linear');
assert_isequal(h, l1);
h = findobj('-regexp', 'Tag', 'quad');
assert_isequal(h, l2);
h = findobj('-property', 'Marker');
assert_istrue(any(h == l1));
assert_istrue(any(h == l2));
assert_istrue(any(h == l3));
%=============================================================================
h = findobj('Type', 'line', '-and', '-not', {'Tag', 'linear', '-or', 'Tag', 'quadratic'});
assert_isequal(h, l3);
h = findobj({'Tag', 'linear', '-or', 'Tag', 'quadratic'}, '-and', {'Type', 'line'});
assert_isequal(numel(h), 2);
assert_istrue(any(h == l1));
assert_istrue(any(h == l2));
h = findobj('Type', 'line', '-and', {'Tag', 'linear', '-xor', 'Tag', 'quadratic'});
assert_isequal(numel(h), 2);
%=============================================================================
h = findobj('Color', 'red');
assert_istrue(any(h == l1));
h = findobj('Color', 'r');
assert_istrue(any(h == l1));
%=============================================================================
ax.HandleVisibility = 'off';
h = findobj(f, 'Type', 'axes');
assert_istrue(isempty(h));
h = findobj(f, 'Type', 'line');
assert_istrue(isempty(h));
ax.HandleVisibility = 'on';
%=============================================================================
h = findobj([f; ax], 'Type', 'line');
assert_isequal(sum(h == l1), 2);
%=============================================================================
assert_checkerror('findobj(''Type'')', _('Invalid findobj expression.'));
assert_checkerror('findobj(gcf(), ''-depth'', -1)', _('Depth must be a nonnegative integer or Inf.'));
%=============================================================================
close all
%=============================================================================
