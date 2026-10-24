%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function run_impl_print_module_header(index, total, moduleName)
  fmt = [newline, '%0', int2str(length(int2str(total))), ...
    'd/%0', int2str(length(int2str(total))), 'd [%s]:', newline];
  fprintf(stdout, fmt, index, total, moduleName);
end
%=============================================================================
