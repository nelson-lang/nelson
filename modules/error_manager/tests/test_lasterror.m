%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
lasterror('reset');
state = execstr('undefined_symbol_for_lasterror', 'errcatch');
asserts.isfalse(state);
%=============================================================================
last = lasterror();
asserts.istrue(isstruct(last));
asserts.isequal(last.message, 'Undefined variable or function: undefined_symbol_for_lasterror');
%=============================================================================
lasterror('reset');
resetLast = lasterror();
asserts.isequal(resetLast.message, '');
%=============================================================================
lasterror(last);
restored = lasterror();
asserts.isequal(restored.message, last.message);
%=============================================================================
