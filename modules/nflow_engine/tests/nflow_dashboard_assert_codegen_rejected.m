%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nflow_dashboard_assert_codegen_rejected(d, target, expected)
  model = [tempname(), '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  rejected = false;
  try
    nflow_codegenerate(model, tempdir(), false, target);
  catch err
    rejected = ~isempty(strfind(err.message, expected));
  end
  assert_istrue(rejected);
end
