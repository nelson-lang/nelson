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
assert_isequal(nargin('ferror'), 2);
assert_isequal(nargout('ferror'), 2);
%=============================================================================
R = ferror(stdout);
assert_isequal(R, '');
%=============================================================================
[R1, R2] = ferror(stdout);
assert_isequal(R1, '');
assert_isequal(R2, 0);
%=============================================================================
filename = [tempdir(), 'test_ferror.csv'];
fid = fopen(filename, 'w');
res = fgets(fid);
[msg, code] = ferror(fid);
fclose(fid);
assert_isequal(code, -1);
assert_istrue(ischar(msg));
assert_isfalse(isempty(msg));
assert_istrue(ndims(msg) == 2 && any(size(msg) <= 1));
%=============================================================================
filename = [tempdir(), 'test_ferror2.csv'];
fid = fopen(filename, 'w');
res = fgets(fid);
[msg1, code1] = ferror(fid, 'clear');
[msg2, code2] = ferror(fid);
fclose(fid);
assert_isequal(code1, -1);
assert_isequal(code2, 0);
assert_istrue(ischar(msg2));
assert_istrue(isempty(msg2));
%=============================================================================
