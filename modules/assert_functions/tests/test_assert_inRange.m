%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.inRange'), 3);
asserts.isequal(nargout('asserts.inRange'), 2);
%=============================================================================
asserts.inRange([1 2], 0, 3);
%=============================================================================
[r, msg] = asserts.inRange([1 4], 0, 3);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.inRange'));
%=============================================================================
