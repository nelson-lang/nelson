%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(iswasm(), 'Requires a native shell and process creation, unavailable in WebAssembly.');
cmd = 'cd';
[a, b] = unix(cmd);
asserts.isequal(a, 0);
%=============================================================================
cmd = 'cd';
[a, b] = system(cmd);
asserts.isequal(a, 0);
%=============================================================================
cmd = 'cd';
[a, b] = dos(cmd);
asserts.isequal(a, 0);
%=============================================================================
