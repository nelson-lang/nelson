%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
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
% <-- Issue URL -->
% https://github.com/Nelson-numerical-software/nelson/issues/541
% <-- Short Description -->
% common class between two elements
%=============================================================================
R = [false pi];
REF = [ 0 pi];
assert_isequal(R, REF);
%=============================================================================
R = [ pi int32(10)];
REF = int32([ 3 10]);
assert_isequal(R, REF);
%=============================================================================
R = [int32(10) uint8(150)];
REF =  int32([10 150]);
assert_isequal(R, REF);
%=============================================================================
R = [false pi int32(10) single(17.82) uint8(150)];
REF =  int32([0     3    10    18   150 ]);
assert_isequal(R, REF);
%=============================================================================
R = [false pi int32(10) {} single(17.82) uint8(150)];
REF = {logical(0), pi, int32(10), single(17.82), uint8(150)};
assert_isequal(R, REF);
%=============================================================================
R = horzcat(false, pi, int32(10), {}, single(17.82), uint8(150));
REF = {logical(0), pi, int32(10), single(17.82), uint8(150)};
assert_isequal(R, REF);
%=============================================================================
R = [single(120) 'f'];
REF = 'xf';
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('[single(120i) ''f'']', _('Complex values cannot be converted to chars.'))
%=============================================================================
R = [{65, {}; 'test1', 2i-8}, {'A', {}; 'test1', 2i-8}];
assert_isequal(class(R), 'cell');
assert_isequal(size(R), [2, 4]);
%=============================================================================
R = int16(4) == int8(4);
REF = true;
assert_isequal(R, REF)
%=============================================================================
R = [int16(450) uint8(250) int32(1000000)];
REF = int16([ 450     250   32767]);
assert_isequal(R, REF);
%=============================================================================
R = [int16(5000) int8(50)];
REF = int16([5000     50]);
assert_isequal(R, REF);
%=============================================================================
R = [int8(50) int16(5000)];
REF = int8([50, 127]);
assert_isequal(R, REF);
%=============================================================================
R = [int8(-100) uint8(100)];
REF = int8([-100 100]);
assert_isequal(R, REF);
%=============================================================================
R = [uint8(100) int8(-100)];
REF = uint8([100, 0]);
assert_isequal(R, REF);
%=============================================================================
R = [100, {uint8(200), 300}, 'Nelson'];
REF = {100, uint8(200), 300, 'Nelson'};
assert_isequal(R, REF);
%=============================================================================
R = [single(4.5) single(-2.8) pi 5.73*10^300];
REF = single([ 4.5000   -2.8000    pi       Inf]);
assert_isequal(R, REF);
%=============================================================================
R = [int8(21) int8(-22) int8(23) pi 45/6];
REF = int8([21   -22    23     3     8]);
assert_isequal(R, REF);
%=============================================================================
R = ['A' 'B' 'C' 68 69 70];
REF = 'ABCDEF';
assert_isequal(R, REF);
%=============================================================================
R = [true false false pi sqrt(7)];
REF = [1 0 0 pi sqrt(7)];
assert_isequal(R, REF);
%=============================================================================
