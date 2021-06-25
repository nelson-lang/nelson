%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
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