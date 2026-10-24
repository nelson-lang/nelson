%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.notEqual'), 2);
asserts.isequal(nargout('asserts.notEqual'), 2);
%=============================================================================
asserts.notEqual(1, 2);
%=============================================================================
[r, msg] = asserts.notEqual(1, 1);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.notEqual'));
%=============================================================================
asserts.checkerror('feval(''asserts.notEqual'', 1, 1)', 'Assertion failed: asserts.notEqual expected different values.');
%=============================================================================
