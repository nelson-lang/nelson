%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function stop = odeCallOutput(problem, t, y, flag)
  stop = false;
  outputFcn = problem.options.OutputFcn;
  if isempty(outputFcn)
    return
  end
  if ~isempty(problem.options.OutputSel) && (isempty(flag) || strcmp(flag, 'init'))
    y = y(problem.options.OutputSel, :);
  end
  args = {t, y, flag};
  if isfield(problem.options, 'Parameters') && ~isempty(problem.options.Parameters)
    args = odeAppendParameters(args, problem.options.Parameters);
  end
  result = feval(outputFcn, args{:});
  stop = odeStopFlag(result, 'OutputFcn');
end
%=============================================================================
