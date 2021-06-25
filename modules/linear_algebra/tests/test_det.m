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
assert_isequal(nargin('det'), 1);
assert_isequal(nargout('det'), 1);
%=============================================================================
R = det([]);
REF = double(1);
assert_isequal(R, REF);
%=============================================================================
R = det(single([]));
REF = single(1);
assert_isequal(R, REF);
%=============================================================================
R = det(NaN);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = det([1, 1; 1,NaN]);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = det([1 1; 1 NaN] + i);
REF = complex(NaN, NaN);
assert_isequal(R, REF);
%=============================================================================
R = det(Inf);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
R = det(Inf);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
R = det(1);
REF = 1;
assert_isequal(R, REF);
%=============================================================================
R = det(1 + i);
REF = 1 + i;
assert_isequal(R, REF);
%=============================================================================
R = det([1 1; 1 2]);
REF = 1;
assert_isequal(R, REF);
%=============================================================================
R = det([1 1; 1 2] + i);
REF = 1 + i;
assert_isequal(R, REF);
%=============================================================================
R = det(sparse([1 1; 1 2]));
REF = 1;
assert_isequal(R, REF);
%=============================================================================
R = det(sparse([1 1; 1 2] + i));
REF = 1 + i;
assert_isequal(R, REF);
%=============================================================================
R = det(0);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
R = det([1 2 ; 1 2]);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = det([1 2])', _('Square matrix expected.'));
assert_checkerror('R = det(''hello'')', sprintf(_('function %s_det undefined.'), 'char'));
%=============================================================================
