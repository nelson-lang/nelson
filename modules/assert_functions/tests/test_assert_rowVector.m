%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.rowVector'), 1);
asserts.isequal(nargout('asserts.rowVector'), 2);
%=============================================================================
asserts.rowVector([1 2]);
%=============================================================================
[r, msg] = asserts.rowVector([1; 2]);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.rowVector'));
%=============================================================================
