%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = needs_sequential_execution(test_case)
  tf = test_case.options.sequential_test_required ...
    || test_case.options.gui_mode ...
    || test_case.options.mpi_mode ...
    || test_case.isbench;
end
%=============================================================================
