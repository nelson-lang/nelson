%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function msg = test_redirect_error_message(redirect_err, res_cmd)
  try
    s = jsondecode(redirect_err, '-file');
    if ~isempty(s) && isfield(s, 'msg')
      msg = s.msg;
      return
    end
  catch
  end
  msg = fileread(redirect_err);
  if isempty(msg)
    msg = ['Error code not caught:', ' ', num2str(res_cmd)];
  end
end
%=============================================================================
