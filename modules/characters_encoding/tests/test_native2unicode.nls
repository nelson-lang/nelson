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
assert_isequal(nargin('native2unicode'), 2);
assert_isequal(nargout('native2unicode'), 1);
%=============================================================================
C = uint8([26   108    26   118   101]);
R = double(native2unicode(C, 'ascii'));
REF = double(C);
assert_isequal(R, REF);
%=============================================================================
R = native2unicode('Nelson', 'ascii');
REF = 'Nelson';
assert_isequal(R, REF);
%=============================================================================
C = uint8([149   208   137   188   150   188]);
R = native2unicode(C, 'SHIFT_JIS');
REF = '片仮名';
assert_isequal(R, REF);
%=============================================================================
C = [101   108   101   118   101 ] + i;
R = native2unicode(C, 'ascii');
REF = 'eleve';
assert_isequal(R, REF);
%=============================================================================
REF = 'Виртуальная';
C = uint8([194   232   240   242   243   224   235   252   237   224   255]);
R = native2unicode(C, 'Windows-1251');
assert_isequal(R, REF);
%=============================================================================
