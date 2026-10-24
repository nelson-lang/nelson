%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('execstr'), 2);
asserts.isequal(nargout('execstr'), 1);
%=============================================================================
status = execstr('execstrValue = 42;', 'errcatch');
asserts.istrue(status);
asserts.isequal(execstrValue, 42);
status = execstr('execstrMissingValue', 'errcatch');
asserts.isfalse(status);
%=============================================================================
