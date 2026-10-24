%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function optimFminuncDisplay(opts, phase, values, message)
  if isNoDisplay(opts)
    return
  end
  if strcmp(phase, 'header') && isIterDisplay(opts)
    disp(' Iteration  Func-count          f(x)       Step-size    First-order opt')
  elseif strcmp(phase, 'iter') && isIterDisplay(opts)
    disp([int2str(values.iteration), '           ', int2str(values.funccount), ...
      '        ', num2str(values.fval, 12), '    ', num2str(values.stepsize, 6), ...
      '       ', num2str(values.firstorderopt, 6)])
  elseif strcmp(phase, 'final') && (isFinalDisplay(opts) || isIterDisplay(opts) || isNotifyDisplay(opts))
    if isIterDisplay(opts), disp(' '); end
    disp(message)
  end
end
%=============================================================================
function tf = isIterDisplay(opts)
  tf = strcmpi(opts.Display, 'iter') || strcmpi(opts.Display, 'iter-detailed');
end
%=============================================================================
function tf = isFinalDisplay(opts)
  tf = strcmpi(opts.Display, 'final') || strcmpi(opts.Display, 'final-detailed');
end
%=============================================================================
function tf = isNotifyDisplay(opts)
  tf = strcmpi(opts.Display, 'notify') || strcmpi(opts.Display, 'notify-detailed');
end
%=============================================================================
function tf = isNoDisplay(opts)
  tf = strcmpi(opts.Display, 'off') || strcmpi(opts.Display, 'none');
end
%=============================================================================
