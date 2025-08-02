%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tempdest = tempname(varargin)
  narginchk(0, 1)
  if nargin == 0
    tempdirdest = tempdir();
  else
    mustBeFolder(varargin{1}, 1);
    tempdirdest = convertStringsToChars(varargin{1});
  end
  if (strcmp(tempdirdest, tempdir()))
    tempdest = [tempdirdest, 'tp', strrep(createGUID,'-','_')];
  else
    tempdest = [tempdirdest, '/', 'tp', strrep(createGUID,'-','_')];
  end
  if isdir(tempdest) || isfile(tempdest)
    tempdest = tempname(tempdirdest);
  end
end
%=============================================================================
