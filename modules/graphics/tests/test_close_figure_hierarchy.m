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
a = gca();
x = linspace(-pi * 7 / 8, pi * 7 / 8, 15);
y = cos(x);
h = plot(x, y, 'ro');
delete(a);
asserts.istrue(isgraphics(f));
asserts.isfalse(isgraphics(a));
asserts.isfalse(isgraphics(h));
%=============================================================================
f = figure();
a = gca();
x = linspace(-pi * 7 / 8, pi * 7 / 8, 15);
y = cos(x);
h = plot(x, y, 'ro');
delete(h);
asserts.istrue(isgraphics(f));
asserts.istrue(isgraphics(a));
asserts.isfalse(isgraphics(h));
%=============================================================================
f = figure();
a = gca();
x = linspace(-pi * 7 / 8, pi * 7 / 8, 15);
y = cos(x);
h = plot(x, y, 'ro');
close(f);
asserts.isfalse(isgraphics(f));
asserts.isfalse(isgraphics(a));
asserts.isfalse(isgraphics(h));
%=============================================================================
