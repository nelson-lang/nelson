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
addpath([nelsonroot(), '/modules/core/tests/']);
%=============================================================================
assert_checkerror('fun_nargoutchk()', _('Wrong number of output arguments.'), 'Nelson:nargoutchk:notEnoughOutputs');
%=============================================================================
assert_checkerror('[a, b, c, d] = fun_nargoutchk()', _('Wrong number of output arguments.'), 'Nelson:nargoutchk:tooManyOutputs');
%=============================================================================
R = nargoutchk(1, 2 , 3);
REF = _('Too many output arguments.');
assert_isequal(R, REF);
%=============================================================================
R = nargoutchk(1, 2 , 0);
REF = _('Not enough output arguments.');
assert_isequal(R, REF);
%=============================================================================
R = nargoutchk(1, 2 , 1);
REF = '';
assert_isequal(R, REF);
%=============================================================================
S = nargoutchk(1, 2 , 3, 'struct');
MSG = _('Too many output arguments.');
ID = 'Nelson:nargoutchk:tooManyOutputs';
assert_isequal(S.message, MSG);
assert_isequal(S.identifier, ID);
%=============================================================================
S = nargoutchk(1, 2 , 0, 'struct');
MSG = _('Not enough output arguments.');
ID = 'Nelson:nargoutchk:notEnoughOutputs';
assert_isequal(S.message, MSG);
assert_isequal(S.identifier, ID);
%=============================================================================
S = nargoutchk(1, 2 , 1, 'struct');
MSG = '';
ID = '';
assert_isequal(S.message, MSG);
assert_isequal(S.identifier, ID);
%=============================================================================
