%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
ax = gca();
img = image();
hold on
P = plot(magic(5));
children1 = ax.Children;
asserts.isequal(size(children1), [6, 1]);
delete(img);
asserts.isequal(size(children1), [6, 1]);
asserts.istrue(~isgraphics(img) && isa(img, 'graphics_object'));
children2 = ax.Children;
asserts.isequal(size(children2), [5, 1]);
%=============================================================================
f = figure();
ax = gca();
h = plot(1:5);
hold on
h(2) = plot(5:-1:1);
asserts.isequal(numel(ax.Children), 2);
delete(h);
asserts.isequal(numel(ax.Children), 0);
asserts.isfalse(any(isgraphics(h)));
%=============================================================================
