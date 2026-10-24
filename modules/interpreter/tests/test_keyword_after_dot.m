%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
s = struct();
s.abort = 1;
s.for = 2;
s.end = 3;
s.global = 4;
s.persistent = 5;
asserts.isequal(s.abort, 1);
asserts.isequal(s.for, 2);
asserts.isequal(s.end, 3);
asserts.isequal(s.global, 4);
asserts.isequal(s.persistent, 5);
%=============================================================================
