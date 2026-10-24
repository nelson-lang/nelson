%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = can_pool_test_case_mode(options)
  tf = options.cli_mode && ~options.gui_mode && ~options.adv_cli_mode && ~options.mpi_mode;
end
%=============================================================================
