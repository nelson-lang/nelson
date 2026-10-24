%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [ax, args, nargs] = axescheck(varargin)
  % Pull an axes handle out of an argument list.
  %   [ax, args, nargs] = axescheck(arg1, arg2, ...)
  % ax is a leading axes handle if one was given (otherwise []), args holds
  % the remaining arguments, and nargs is numel(args). A trailing
  % 'Parent', value pair also supplies the axes and is removed from args.
  args = varargin;
  ax = [];
  % A leading scalar axes handle.
  if ~isempty(args) && isscalar(args{1}) && isAxesHandleLocal(args{1})
    ax = args{1};
    args = args(2:end);
  end
  % A 'Parent', value pair anywhere in the list (the last one wins).
  k = 1;
  while k < numel(args)
    name = args{k};
    if (ischar(name) || (isstring(name) && isscalar(name))) && strcmpi(name, 'Parent')
      ax = args{k + 1};
      args(k:k + 1) = [];
    else
      k = k + 1;
    end
  end
  nargs = numel(args);
end
%=============================================================================
function tf = isAxesHandleLocal(value)
  tf = false;
  try
    tf = isgraphics(value, 'axes') || isgraphics(value, 'polaraxes');
  catch
    tf = false;
  end
end
%=============================================================================
