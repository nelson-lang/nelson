%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.lessOrEqual'), 2);
asserts.isequal(nargout('asserts.lessOrEqual'), 2);
%=============================================================================
asserts.lessOrEqual([1 2], [1 2]);
%=============================================================================
[r, msg] = asserts.lessOrEqual([1 4], 3);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.lessOrEqual'));
%=============================================================================
