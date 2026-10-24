%=============================================================================
% Copyright (c) 2017-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
S = getLastReport();
asserts.isequal(S, '');
%=============================================================================
execstr('a', 'errcatch');
S = getLastReport();
REF = '
Error in execstr
Undefined variable or function: a

';
asserts.istrue(startsWith(S, REF));
% Reports may retain a short absolute filename and the caller's stack frames.
% Check the failing script and line independently of its installation path.
location = regexp(S, 'at line[ ]+15 of ''[^''\r\n]*test_getLastReport\.m''', 'once');
asserts.isfalse(isempty(location));
%=============================================================================
