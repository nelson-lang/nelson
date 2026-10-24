%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function optimLsqDisplay(opts, phase, style, values)
  % Iteration and final display for the least-squares solvers
  % (lsqnonlin, fsolve). style: 'trr', 'lm' or 'dogleg'.
  if isNoDisplay(opts)
    return
  end
  switch phase
    case 'header'
      if ~isIterDisplay(opts)
        return
      end
      disp(' ');
      switch style
        case 'lm'
          disp('                                        First-order                     Norm of ');
          disp(' Iteration  Func-count      Resnorm      optimality       Lambda           step');
        case 'dogleg'
          disp('                                         Norm of      First-order   Trust-region');
          disp(' Iteration  Func-count     ||f(x)||^2        step       optimality       radius');
        otherwise
          disp('                                            Norm of      First-order ');
          disp(' Iteration  Func-count      Resnorm            step       optimality');
      end
    case 'iter'
      if ~isIterDisplay(opts)
        return
      end
      disp(iterLine(style, values));
    case 'final'
      show = isFinalDisplay(opts) || isIterDisplay(opts) || ...
        (isNotifyDisplay(opts) && values.exitflag <= 0);
      if ~show
        return
      end
      if isIterDisplay(opts)
        disp(' ');
      end
      if isDetailedDisplay(opts)
        disp(values.messageDetailed);
      else
        disp(values.message);
      end
  end
end
%=============================================================================
function line = iterLine(style, values)
  switch style
    case 'lm'
      line = sprintf('%6d %11d %16.6g %15.6g %12.6g', values.iteration, ...
        values.funccount, values.resnorm, values.firstorderopt, values.lambda);
      if values.iteration > 0
        line = [line, sprintf(' %14.6g', values.stepsize)];
      end
    case 'dogleg'
      line = sprintf('%6d %11d %15.6g', values.iteration, values.funccount, ...
        values.resnorm);
      if values.iteration > 0
        line = [line, sprintf(' %11.6g', values.stepsize)];
      else
        line = [line, repmat(' ', 1, 12)];
      end
      line = [line, sprintf(' %16.6g %12.6g', values.firstorderopt, values.radius)];
    otherwise
      line = sprintf('%6d %11d %17.6g', values.iteration, values.funccount, ...
        values.resnorm);
      if values.iteration > 0
        line = [line, sprintf(' %15.6g', values.stepsize)];
      else
        line = [line, repmat(' ', 1, 16)];
      end
      line = [line, sprintf(' %16.6g', values.firstorderopt)];
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
function tf = isDetailedDisplay(opts)
  display = lower(opts.Display);
  tf = length(display) > 9 && strcmp(display(end - 8:end), '-detailed');
end
%=============================================================================
