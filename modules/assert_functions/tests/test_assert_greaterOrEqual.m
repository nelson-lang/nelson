%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.greaterOrEqual'), 2);
asserts.isequal(nargout('asserts.greaterOrEqual'), 2);
%=============================================================================
asserts.greaterOrEqual([2 3], [2 3]);
%=============================================================================
[r, msg] = asserts.greaterOrEqual([0 3], 1);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.greaterOrEqual'));
%=============================================================================
