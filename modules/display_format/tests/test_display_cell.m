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
A = {'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaagggggggggggggggrrgrgrgrgggggggggggggggggggggggggggrgrgr';'vvvvvvvvvvvvvvvvvvvvvvvvvv';'ddddddddddddddddddd'}
R = evalc('A');
REF = '
A =

  3×1 cell array

    {''aaaaaaaaaaaaaaaaaaaaaaaaaaaaaagggggggggggggggrrgrgrgrgggggggggggggggggg…''}
    {''vvvvvvvvvvvvvvvvvvvvvvvvvv''}                                              
    {''ddddddddddddddddddd''}                                                     

';
assert_isequal(R, REF)
%=============================================================================
A = {1,2,3;
     'text',rand(5,10,2),{11; 22; 33}};
R = evalc('A')
REF =  '
A =

  2×3 cell array

    {[1]}       {[2]}              {[3]}     
    {''text''}    {5×10×2 double}    {3×1 cell}

';
assert_isequal(R, REF)
%=============================================================================
A = {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaagggggggggggggggrrgrgrgrgggggggggggggggggggggggggggrgrgr";"vvvvvvvvvvvvvvvvvvvvvvvvvv";"ddddddddddddddddddd"};
R = evalc('A');
REF =  '
A =

  3×1 cell array

    {1×1 string}                    
    {["vvvvvvvvvvvvvvvvvvvvvvvvvv"]}
    {["ddddddddddddddddddd"]}       

';
assert_isequal(R, REF)
%=============================================================================
A = cell(3);
R = evalc('A');
REF =  '
A =

  3×3 cell array

    {0×0 double}    {0×0 double}    {0×0 double}
    {0×0 double}    {0×0 double}    {0×0 double}
    {0×0 double}    {0×0 double}    {0×0 double}

';
assert_isequal(R, REF)
%=============================================================================

