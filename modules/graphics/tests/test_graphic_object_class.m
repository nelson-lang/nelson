%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--ENGLISH IMPOSED-->
%=============================================================================
c = groot;
assert_isequal(class(c), 'root');
assert_isequal(size(c), [1 1]);
%=============================================================================
a = figure(1);
assert_isequal(class(a), 'figure');
assert_isequal(size(a), [1 1]);
%=============================================================================
d = [a, a];
assert_isequal(class(d), 'figure');
assert_isequal(size(d), [1 2]);
%=============================================================================
e = [c, c];
assert_isequal(class(e), 'root');
assert_isequal(size(e), [1 2]);
%=============================================================================
f = [a,c];
assert_isequal(class(f), 'graphic_object');
assert_isequal(size(f), [1 2]);
%=============================================================================
g = [c, a];
assert_isequal(class(g), 'graphic_object');
assert_isequal(size(g), [1 2]);
%=============================================================================
k = [c; a];
assert_isequal(class(k), 'graphic_object');
assert_isequal(size(k), [2 1]);
%=============================================================================
assert_checkerror('kk = [c, 1];', 'function graphic_object_horzcat_double undefined.');
assert_checkerror('kk = [c; 1];', 'function graphic_object_vertcat_double undefined.');
%=============================================================================