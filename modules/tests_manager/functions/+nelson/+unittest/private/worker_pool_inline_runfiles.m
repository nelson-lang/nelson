%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function content = worker_pool_inline_runfiles(chunk)
  content = cell(length(chunk) * 2, 1);
  for k = 1:length(chunk)
    content{(k - 1) * 2 + 1} = ...
      ['res_struct = worker_pool_safe_runfile(', nelsonStringLiteral(chunk(k).filename), ');'];
    content{(k - 1) * 2 + 2} = ...
      'disp([''__NELSON_UNITTEST_RESULT__'', jsonencode(encode_runfile_payload(res_struct))]);';
  end
end
%=============================================================================
