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
assert_isequal(nargin('sha256'), 2);
assert_isequal(nargout('sha256'), 1);
%=============================================================================
R = sha256('Nelson');
REF = '818d0e5ffae3c30dfbe68dbb57896728bc1598d0094467bb378db4cb21a1774c';
assert_isequal(R, REF)
%=============================================================================
R = sha256({'Hello', 'World'});
REF = {'185f8db32271fe25f561a6fc938b2e264306ec304eda518007d1764826381969', ...
'78ae647dc5544d227130a0682a51e30bc7777fbb6d8a8f17007463a3ecd1d524'};
assert_isequal(R, REF)
%=============================================================================
R = sha256(["Hello"; "World"]);
REF = ["185f8db32271fe25f561a6fc938b2e264306ec304eda518007d1764826381969";
"78ae647dc5544d227130a0682a51e30bc7777fbb6d8a8f17007463a3ecd1d524"];
assert_isequal(R, REF)
%=============================================================================
R = sha256(["Hello"; "World"], '-file');
REF = [""; ""];
assert_isequal(R, REF)
%=============================================================================
R = sha256({'Hello'; 'World'}, '-file');
REF = {''; ''};
assert_isequal(R, REF)
%=============================================================================
R = sha256([modulepath('matio'), '/tests/mat/test_char_array_unicode_7.4_GLNX86.mat']);
REF = 'f7ef31cc009995e0643236676ebf2de0d72cf41ea66f3ace3436e53e7b5e04f5';
assert_isequal(R, REF)
%=============================================================================
R = sha256([modulepath('matio'), '/tests/mat/test_char_array_unicode_7.4_GLNX86.mat'], '-file');
REF = 'f7ef31cc009995e0643236676ebf2de0d72cf41ea66f3ace3436e53e7b5e04f5';
assert_isequal(R, REF)
%=============================================================================
