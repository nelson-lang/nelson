%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function scriptFile = worker_pool_write_script(chunk, privateDir)
  scriptFile = [unittest_tempdir(), 'nelson_unittest_worker_', createGUID(), '.m'];
  filewrite(scriptFile, worker_pool_script_content(chunk, privateDir));
end
%=============================================================================
