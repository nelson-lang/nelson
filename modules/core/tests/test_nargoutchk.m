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
asserts.checkerror('fun_nargoutchk()', _('Not enough output arguments.'), 'Nelson:nargoutchk:notEnoughOutputs');
%=============================================================================
asserts.checkerror('[a, b, c, d] = fun_nargoutchk()', _('Too many output arguments.'), 'Nelson:nargoutchk:tooManyOutputs');
%=============================================================================
R = nargoutchk(1, 2, 3);
REF = _('Too many output arguments.');
asserts.isequal(R, REF);
%=============================================================================
R = nargoutchk(1, 2, 0);
REF = _('Not enough output arguments.');
asserts.isequal(R, REF);
%=============================================================================
R = nargoutchk(1, 2, 1);
REF = '';
asserts.isequal(R, REF);
%=============================================================================
S = nargoutchk(1, 2, 3, 'struct');
MSG = _('Too many output arguments.');
ID = 'Nelson:nargoutchk:tooManyOutputs';
asserts.isequal(S.message, MSG);
asserts.isequal(S.identifier, ID);
%=============================================================================
S = nargoutchk(1, 2, 0, 'struct');
MSG = _('Not enough output arguments.');
ID = 'Nelson:nargoutchk:notEnoughOutputs';
asserts.isequal(S.message, MSG);
asserts.isequal(S.identifier, ID);
%=============================================================================
S = nargoutchk(1, 2, 1, 'struct');
MSG = '';
ID = '';
asserts.isequal(S.message, MSG);
asserts.isequal(S.identifier, ID);
%=============================================================================
addpath([modulepath('functions_manager'), '/tests']);
runtime_nargoutchk_fixture();
[out1, out2] = runtime_nargoutchk_fixture();
asserts.isequal([out1, out2], [1 2]);
asserts.checkerror('[out1, out2, out3] = runtime_nargoutchk_fixture()', _('Too many output arguments.'), 'Nelson:nargoutchk:tooManyOutputs');
nargoutMessage = nargoutchk(1, 2, 3);
asserts.isequal(nargoutMessage, _('Too many output arguments.'));
nargoutStruct = nargoutchk(1, 2, 0, 'struct');
asserts.isequal(nargoutStruct.identifier, 'Nelson:nargoutchk:notEnoughOutputs');
%=============================================================================
