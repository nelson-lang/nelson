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
C = {{42}};
R = evalc('celldisp(C)');
REF = ' 
C{1}{1} =
 
    42

 
';
assert_isequal(R, REF)
%=============================================================================
C = {'row1',[1 2 3],3+4i;
     'row2',[2 4;1 3],{'innercells',42}};
R = evalc('celldisp(C)');
REF = ' 
C{1,1} =
 
row1
 
 
C{2,1} =
 
row2
 
 
C{1,2} =
 
     1     2     3

 
 
C{2,2} =
 
     2     4
     1     3

 
 
C{1,3} =
 
   3.0000 + 4.0000i

 
 
C{2,3}{1} =
 
innercells
 
 
C{2,3}{2} =
 
    42

 
';
assert_isequal(R, REF)
%=============================================================================


