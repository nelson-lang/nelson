%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = writematrix(varargin)
  nargoutchk(0, 0);
  M = varargin{1};
  if ischar(M)
    M = string(M);
  end
  if (nargin < 2)
    name = inputname(1);
    filename = getDefaultFilename(inputname(1));
  else
    filename = varargin{2};
    mustBeTextScalar(filename, 2);
    filename = convertStringsToChars(filename);
  end
  
  is_text = strcmp(fileparts(filename, 'extension'), '.txt') || strcmp(fileparts(filename, 'extension'), '.csv') || strcmp(fileparts(filename, 'extension'), '.dat');
  if (nargin < 3 && (isnumeric(M) || islogical(M)) && is_text)
    % dlmwrite is faster than convert to table and write
    if ispc()
      eol = 'pc';
    else
      eol = 'unix';
    end
    dlmwrite(filename, M, ',', 0, 0, eol, '%.15g');
    return;
  end
  
  if (nargin > 3 && mod(nargin - 2, 2) ~= 0)
    error(_('Incorrect number of arguments.'));
  end
  
  checkRestrictedOptions(varargin);
  if (nargin > 2)
    args = varargin(3:end);
  else
    args = {};
  end
  writeData(M, filename, args);
  varargout = {};
end
%=============================================================================
function checkRestrictedOptions(varargin)
  for i = 3:2:nargin
    if strcmp('WriteVariableNames', convertStringsToChars(varargin{i}))
      error(_('WriteVariableNames not allowed.'));
    end
    if strcmp('WriteRowNames', convertStringsToChars(varargin{i}))
      error(_('WriteRowNames not allowed.'));
    end
  end
end
%=============================================================================
function writeData(M, filename, args)
  T = table(M);
  writetable(T, filename, args{:}, 'WriteVariableNames', false, 'WriteRowNames', false);
end
%=============================================================================
function filename = getDefaultFilename(name)
  if isempty(name)
    filename = "matrix.txt";
  else
    filename = name + ".txt";
  end
end
%=============================================================================
