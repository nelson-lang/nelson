%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function open_system(modelOrFile)
  % open_system  Open the nflow editor on a model, a model file, or an SSP.
  %   open_system(name)    open a model loaded in memory (by name or handle)
  %   open_system(file)    open a .nflow model file
  %   open_system(sspFile) import an SSP archive (.ssp) to a runnable model and
  %                        open it
  %
  %   An SSP is not a diagram: it is imported with NFlow.sspImport (its component
  %   FMUs are extracted, wired and written to a .nflow) and the resulting model
  %   is opened. Handles and in-memory model names are passed straight through.
  narginchk(1, 1);
  if ischar(modelOrFile) || (isstring(modelOrFile) && isscalar(modelOrFile))
    p = char(modelOrFile);
    [~, stem, ext] = fileparts(p);
    if strcmpi(ext, '.ssp') && isfile(p)
      dest = [tempdir(), stem, '_ssp'];
      if ~isdir(dest)
        mkdir(dest);
      end
      modelOrFile = NFlow.sspImport(p, dest);
    end
  end
  __open_system__(modelOrFile);
end
%=============================================================================
