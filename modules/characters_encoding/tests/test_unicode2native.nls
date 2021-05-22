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
assert_isequal(nargin('unicode2native'), 2);
assert_isequal(nargout('unicode2native'), 1);
%=============================================================================
R1 = unicode2native('éléve');
R2 = unicode2native('éléve', '');
assert_isequal(R1, R2);
%=============================================================================
R = unicode2native('azerty', 'ascii');
REF = uint8([97   122   101   114   116   121]);
assert_isequal(R, REF);
%=============================================================================
R = unicode2native('éléve', 'ascii');
REF = uint8([26   108    26   118   101]);
assert_isequal(R, REF);
%=============================================================================
R = unicode2native('éléve', 'UTF-8');
REF = uint8([195   169   108   195   169   118   101]);
assert_isequal(R, REF);
%=============================================================================
R = unicode2native('éléve', 'Windows-1252');
REF = uint8([233   108   233   118   101]);
assert_isequal(R, REF);
%=============================================================================
R = unicode2native ("Виртуальная", 'Windows-1251');
REF = uint8([194   232   240   242   243   224   235   252   237   224   255]);
assert_isequal(R, REF);
%=============================================================================
R = unicode2native('片仮名', 'SHIFT_JIS');
REF = uint8([149   208   137   188   150   188]);
%=============================================================================
assert_checkerror('R = unicode2native(''Виртуальная'', ''_M$_'');', [_('Invalid charset: '), '_M$_'])
%=============================================================================
