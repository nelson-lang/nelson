%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% An arguments-block default may span several physical lines, either through
% an explicit "..." continuation or an implicit one where brackets stay open
% across a newline (matrix rows). The pre-lexer rewrite used to process one
% physical line per declaration, so a multi-line default was truncated and the
% function failed to parse. This locks the corrected behavior.
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/arguments']);
%=============================================================================
% a = [1 2; 3 4] (sum 10), b = max(10, 20) = 20, c = [100; 200] (sum 300).
asserts.isequal(arguments_edge_multiline_default(), 330);
%=============================================================================
rmpath([nelsonroot(), '/modules/interpreter/tests/arguments']);
%=============================================================================
