%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.columnVector'), 1);
asserts.isequal(nargout('asserts.columnVector'), 2);
%=============================================================================
asserts.columnVector([1; 2]);
%=============================================================================
[r, msg] = asserts.columnVector([1 2]);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.columnVector'));
%=============================================================================
