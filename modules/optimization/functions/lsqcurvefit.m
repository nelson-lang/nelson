%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, resnorm, residual, exitflag, output, lambda, jacobian] = lsqcurvefit(varargin)
  narginchk(4, 7);
  nargoutchk(0, 7);
  fun = varargin{1};
  x0 = varargin{2};
  xdata = varargin{3};
  ydata = varargin{4};
  tail = varargin(5:end);
  if length(tail) >= 3 && isstruct(tail{3})
    tail{3} = lsqcurvefitNormalizeOptions(tail{3});
  end
  residualFun = @(x) fun(x, xdata) - ydata;
  [x, resnorm, residual, exitflag, output, lambda, jacobian] = lsqnonlin(residualFun, x0, tail{:});
end
%=============================================================================
function opts = lsqcurvefitNormalizeOptions(opts)
  if ~isfield(opts, 'Algorithm') || isempty(opts.Algorithm) || strcmpi(opts.Algorithm, 'default')
    opts.Algorithm = 'trust-region-reflective';
  end
end
%=============================================================================
