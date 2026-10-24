%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = optimFminuncIsLBFGS(opts)
  value = opts.HessianApproximation;
  if iscell(value)
    tf = strcmpi(char(value{1}), 'lbfgs');
  elseif ischar(value) || isstring(value)
    tf = strcmpi(char(value), 'lbfgs');
  else
    tf = false;
  end
end
%=============================================================================
