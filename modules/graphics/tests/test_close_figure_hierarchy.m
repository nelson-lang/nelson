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
a = gca();
x = linspace(-pi*7/8,pi*7/8,15);
y = cos(x);
h = plot(x, y, 'ro');
delete(a);
assert_istrue(isgraphics(f));
assert_isfalse(isgraphics(a));
assert_isfalse(isgraphics(h));
%=============================================================================
f = figure();
a = gca();
x = linspace(-pi*7/8,pi*7/8,15);
y = cos(x);
h = plot(x, y, 'ro');
delete(h);
assert_istrue(isgraphics(f));
assert_istrue(isgraphics(a));
assert_isfalse(isgraphics(h));
%=============================================================================
f = figure();
a = gca();
x = linspace(-pi*7/8,pi*7/8,15);
y = cos(x);
h = plot(x, y, 'ro');
close(f);
assert_isfalse(isgraphics(f));
assert_isfalse(isgraphics(a));
assert_isfalse(isgraphics(h));
%=============================================================================
