%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ls(varargin)
  nargoutchk(0, 1);
  opts = {};
  for k = 1:nargin
    opts = [opts, convertStringsToChars(varargin{k})];
  end 
  try
    if isunix()
      if (nargout == 0)
        unixLs(nargout, opts);
      else
        varargout{1} = unixLs(nargout, opts);
      end
    else
      if (nargout == 0)
        windowsLs(nargout, opts);
      else
        varargout{1} = windowsLs(nargout, opts);
      end
    end
  catch ME
    throw(ME);
  end
end
% ======================================================
function listing = windowsLs(nOut, opts)
  if numel(opts) > 1
    error('Nelson:ls:TooManyInputArguments', _('Too many input arguments.'));
  end
  if numel(opts) == 0
    lsPath = './';
  else
    lsPath = opts{1};
  end
  listing = char([]);
  if nOut > 0
    d = dir(lsPath);
    listing = char(d.name);
  else
    dir(lsPath);
  end
end
% ======================================================
function varargout = unixLs(nOut, opts)
  nOpts = numel(opts);
  lsPath = './';    
  if numel(opts) == 0
    args = {};
  else
    if (isfolder(opts{1}) || isfile(opts{1}))
      lsPath = opts{1};
      args = opts(2:end);
    else
      args = opts(1:end);
    end
  end
  cmd = ['cd "', lsPath, '" < /dev/null && ls'];
  if ~isempty(args)
    for arg = args
      cmd = [cmd, ' ', arg{1}];
    end
  end
  [s, listing] = system(cmd);
  if s ~= 0
    error('Nelson:ls:OSError', listing);
  end
  if nOut > 0
    varargout{1} = listing;
  else
    disp(listing);
  end
end
% ======================================================
