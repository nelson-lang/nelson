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
assert_isequal(nargin('dbstack'), -1);
assert_isequal(nargout('dbstack'), -1);
%=============================================================================
dbstack();
dbstack('-completenames');
%=============================================================================
[st, idx] = dbstack();
ref_st_1.file = 'test_dbstack.m';
ref_st_1.name = 'test_dbstack';
ref_st_1.line = 32;
assert_isequal(st(1), ref_st_1);
ref_st_2.file = '';
ref_st_2.name = 'run';
ref_st_2.line = zeros(0, 1);
assert_isequal(st(2), ref_st_2);
%=============================================================================
dbstack();
dbstack('-completenames');
%=============================================================================
addpath(fileparts(nfilename('fullpathext'), 'path'));
[st, idx] = fun_dbstack();
%=============================================================================
ref_st_3.file = '';
ref_st_3.name = 'run';
ref_st_3.line = zeros(0, 1);
assert_isequal(st(3), ref_st_3);
%=============================================================================
ref_st_2.file = 'test_dbstack.m';
ref_st_2.name = 'test_dbstack';
ref_st_2.line = 46;
assert_isequal(st(2), ref_st_2);
%=============================================================================
ref_st_1.file = 'fun_dbstack.m';
ref_st_1.name = 'fun_dbstack';
ref_st_1.line = 29;
assert_isequal(st(1), ref_st_1);
%=============================================================================
[st, idx] = dbstack(1);
ref_st_1.file = '';
ref_st_1.name = 'run';
ref_st_1.line = zeros(0, 1);
assert_isequal(st(1), ref_st_1);
%=============================================================================
