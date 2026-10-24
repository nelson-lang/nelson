%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function content = worker_pool_script_loop()
  content = {
    'for worker_k = 1:length(worker_files)'
    '  res_struct = worker_pool_safe_runfile(worker_files{worker_k});'
    '  disp([''__NELSON_UNITTEST_RESULT__'', jsonencode(encode_runfile_payload(res_struct))]);'
    'end'
    'exit(0, ''force'');'
  };
end
%=============================================================================
