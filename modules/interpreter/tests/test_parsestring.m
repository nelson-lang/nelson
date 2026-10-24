%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(parsestring('a = 1;'), 'script');
functionText = sprintf(['function y = parser_direct_f(x)\n', ...
  'y = x;\n', ...
  'end']);
asserts.isequal(parsestring(functionText), 'function');
% a signature-only definition ending at end of file is a valid function
asserts.isequal(parsestring('function y = broken_function(x)'), 'function');
asserts.isequal(parsestring('function y = broken_function(x'), 'error');
%=============================================================================
