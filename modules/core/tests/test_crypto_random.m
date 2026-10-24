%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('crypto.random'), 2);
asserts.isequal(nargout('crypto.random'), 1);
%=============================================================================
R = crypto.random(32);
asserts.isequal(class(R), 'uint8');
asserts.isequal(size(R), [1, 32]);
asserts.isequal(size(crypto.random(1)), [1, 1]);
asserts.isequal(size(crypto.random(1000)), [1, 1000]);
%=============================================================================
% hexadecimal output
H = crypto.random(16, '-hex');
asserts.isequal(class(H), 'char');
asserts.isequal(length(H), 32);
asserts.istrue(all(ismember(H, '0123456789abcdef')));
%=============================================================================
% successive draws differ (astronomically unlikely to be equal by chance)
asserts.isfalse(isequal(crypto.random(32), crypto.random(32)));
%=============================================================================
% argument errors
asserts.checkerror('crypto.random()', _('Not enough input arguments.'));
asserts.checkerror('crypto.random(0)', _('Number of bytes must be an integer between 1 and 1048576.'), 'Nelson:core:randomByteCountExpected');
asserts.checkerror('crypto.random(2.5)', _('Number of bytes must be an integer between 1 and 1048576.'), 'Nelson:core:randomByteCountExpected');
asserts.checkerror('crypto.random(16, ''-bin'')', sprintf(_('#%d argument: ''-hex'' expected.'), 2), 'Nelson:core:cryptoHexOptionExpected');
%=============================================================================
