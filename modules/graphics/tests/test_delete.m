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
f = figure();
ax = gca();
img = image();
hold on
P = plot(magic(5));
children1 = ax.Children;
assert_isequal(size(children1), [6, 1]);
delete(img);
assert_isequal(size(children1), [6, 1]);
assert_istrue(~isgraphics(img) && isa(img, 'graphics_object'));
children2 = ax.Children;
assert_isequal(size(children2), [5, 1]);
%=============================================================================
