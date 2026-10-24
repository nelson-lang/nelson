%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.lessThan'), 2);
asserts.isequal(nargout('asserts.lessThan'), 2);
%=============================================================================
asserts.lessThan([1 2], 3);
%=============================================================================
[r, msg] = asserts.lessThan([1 4], 3);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.lessThan'));
%=============================================================================
[r, msg] = asserts.lessThan(ones(2, 2), ones(1, 3));
asserts.isfalse(r);
asserts.istrue(contains(msg, 'compatible sizes'));
%=============================================================================
