%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.greaterThan'), 2);
asserts.isequal(nargout('asserts.greaterThan'), 2);
%=============================================================================
asserts.greaterThan([2 3], 1);
%=============================================================================
[r, msg] = asserts.greaterThan([0 3], 1);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.greaterThan'));
%=============================================================================
