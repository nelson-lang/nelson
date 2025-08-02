%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
