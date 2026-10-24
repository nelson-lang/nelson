%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [args, runtimeOptions] = extractRuntimeOptions(args)
  runtimeOptions = struct('Timeout', [], 'LauncherGrace', 0, 'WorkerPoolInline', iswasm());
  if isempty(args)
    return
  end
  last = args{end};
  if isstruct(last) && isfield(last, 'nelson_unittest_internal_options')
    runtimeOptions = extractRuntimeOptionsStruct(runtimeOptions, last);
    args(end) = [];
  end
end
