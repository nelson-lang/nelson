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
h = gobjects();
asserts.isequal(size(h), [1 1]);
asserts.isequal(class(h), 'nelson.graphics.GraphicsPlaceholder');
asserts.istrue(isa(h, 'handle'));
asserts.istrue(isa(h, 'nelson.graphics.Graphics'));
asserts.isfalse(isgraphics(h));
asserts.isfalse(ishandle(h));
%=============================================================================
h = gobjects(0);
asserts.isequal(size(h), [0 0]);
asserts.isequal(class(h), 'nelson.graphics.GraphicsPlaceholder');
%=============================================================================
h = gobjects(3);
asserts.isequal(size(h), [3 3]);
asserts.isequal(class(h), 'nelson.graphics.GraphicsPlaceholder');
%=============================================================================
h = gobjects([5]);
asserts.isequal(size(h), size(zeros([5])));
asserts.isequal(size(h), [5 5]);
asserts.isequal(class(h), 'nelson.graphics.GraphicsPlaceholder');
%=============================================================================
h = gobjects([2 3]);
asserts.isequal(size(h), [2 3]);
asserts.isequal(class(h), 'nelson.graphics.GraphicsPlaceholder');
%=============================================================================
h = gobjects([2 3 4]);
asserts.isequal(size(h), [2 3 4]);
asserts.isequal(class(h), 'nelson.graphics.GraphicsPlaceholder');
%=============================================================================
h = gobjects(2, 3, 4);
asserts.isequal(size(h), [2 3 4]);
asserts.isequal(class(h), 'nelson.graphics.GraphicsPlaceholder');
%=============================================================================
asserts.checkerror('gobjects([2 -1])', getString(message('Nelson:gobjects:invalidDimensions')), 'Nelson:gobjects:invalidDimensions');
%=============================================================================
asserts.checkerror('gobjects([2 1.5])', getString(message('Nelson:gobjects:invalidDimensions')), 'Nelson:gobjects:invalidDimensions');
%=============================================================================
h = gobjects(2, 3);
asserts.isequal(size(h), [2 3]);
asserts.isequal(class(h), 'nelson.graphics.GraphicsPlaceholder');
asserts.istrue(isa(h, 'handle'));
asserts.istrue(isa(h, 'nelson.graphics.Graphics'));
tf = isgraphics(h);
asserts.isfalse(any(tf(:)));
tf = ishandle(h);
asserts.isfalse(any(tf(:)));
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
h(1) = ax;
asserts.isequal(h(1), ax);
asserts.isequal(class(h), 'nelson.graphics.Graphics');
asserts.isequal(class(h(1)), 'nelson.graphics.axis.Axes');
asserts.istrue(isgraphics(h(1), 'axes'));
asserts.istrue(isa(h(1), 'nelson.graphics.Graphics'));
tf = ishandle(h);
asserts.isequal(tf, logical([1 0 0; 0 0 0]));
%=============================================================================
% a table column of graphics handles can be vertically concatenated: the
% concatenation widens the placeholder class to its base, which must not be
% rejected as an unsupported variable type.
G1 = table(); G1.obj = gobjects(2, 1); G1.v = [1; 2];
G2 = table(); G2.obj = gobjects(3, 1); G2.v = [3; 4; 5];
RG = [G1; G2];
asserts.isequal(height(RG), 5);
asserts.isequal(RG.v, [1; 2; 3; 4; 5]);
asserts.isequal(numel(RG.obj), 5);
%=============================================================================
close all
%=============================================================================
