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
A = {'Smith','Burns'; 'Jones','Matthews'; 'Peterson','Adams'};
R = sort(A);
REF = {'Jones', 'Adams';
'Peterson', 'Burns';
'Smith', 'Matthews'};
assert_isequal(R, REF);
%=============================================================================
A = {'Smith','Burns'; 'Jones','Matthews'; 'Peterson','Adams'};
[R, I] = sort(A);
REF = [2     3;
     3     1;
     1     2];
assert_isequal(I, REF);
%=============================================================================
A = {'Smith';'Burns'; 'Jones';'Matthews'; 'Peterson';'Adams'};
R = sort(A);
REF = { 'Adams'; 'Burns'; 'Jones'; 'Matthews'; 'Peterson'; 'Smith'};
assert_isequal(R, REF);
%=============================================================================
A = {'Smith';'Burns'; 'Jones';'Matthews'; 'Peterson';'Adams'};
[R, I] = sort(A);
REF = [     6;     2;     3;     4;     5;     1];
assert_isequal(I, REF);
%=============================================================================
A = {'Smith','Burns', 'Jones','Matthews', 'Peterson','Adams'};
R = sort(A);
REF = {'Adams','Burns','Jones','Matthews','Peterson','Smith'};
assert_isequal(R, REF);
%=============================================================================
A = {'Smith','Burns', 'Jones','Matthews', 'Peterson','Adams'};
[R, I] = sort(A);
REF = [ 6     2     3     4     5     1];
assert_isequal(I, REF);
%=============================================================================
A = {'Smith','Burns', 'Jones','Matthews', 'Peterson','Adams'};
assert_checkerror('[R, I] = sort(A, 1)', _('Only one input parameter is supported for cell arrays.'));
%=============================================================================
