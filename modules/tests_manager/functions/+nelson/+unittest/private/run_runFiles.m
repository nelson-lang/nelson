%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [status, raw] = run_runFiles(files, options)
  internalOptions = run_internalOptions(options);
  if options.StopOnFail
    [status, raw] = run_impl(files, '-stoponfail', '', internalOptions);
  else
    [status, raw] = run_impl(files, '', internalOptions);
  end
end
