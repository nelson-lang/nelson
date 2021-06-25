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
assert_isequal(nargin('contains'), -3);
assert_isequal(nargout('contains'), 1);
%=============================================================================
c1 = 'time';
c2 = {'Once','upon'; 'a','time'};
res = contains(c1, c2);
ref = true;
assert_isequal(res, ref);
%=============================================================================
res = contains(c2, c1);
ref = [false, false; false, true];
assert_isequal(res, ref);
%=============================================================================
c1 = {'abs.docx','data.gz','code.m','results.nlf'};
c2 = {'abs','data'};
res = contains(c1, c2);
ref = [true, true, false, false];
assert_isequal(res, ref);
%=============================================================================
c1 = {'abs.docx','data.gz','ABS.m','DAta.nlf'};
c2 = {'abs','data'};
res = contains(c1, c2, 'IgnoreCase', true);
ref = [true, true, true, true];
assert_isequal(res, ref);
%=============================================================================
c1 = {'abs.docx','data.gz','code.m','results.nlf'};
c2 = {1};
assert_checkerror('res = contains(c1, c2);', _('char vector or cell of strings expected.'));
%=============================================================================
c1 = "time";
c2 = ["Once", "upon"; "a", "time"];
res = contains(c1, c2);
ref = true;
assert_isequal(res, ref);
%=============================================================================
c1 = ["abs.docx", "data.gz"; "ABS.m", "DAta.nlf"];
c2 = ["abs"; "data"];
res = contains(c1, c2, 'IgnoreCase', true);
ref = [true, true; true, true];
assert_isequal(res, ref);
%=============================================================================