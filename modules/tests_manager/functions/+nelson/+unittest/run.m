%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = run(varargin)
  [varargin, compatibility] = run_extractCompatibility(varargin); %#ok<NLS0004>
  if compatibility
    [varargin, launcher] = run_extractLauncher(varargin); %#ok<NLS0004>
    cleanupLauncher = run_applyLauncherBackend(launcher); %#ok<NASGU,NLS0004>
    status = run_impl(varargin{:});
    if nargout > 0
      varargout{1} = status;
    end
    return
  end
  results = run_runModern(varargin{:});
  if nargout == 0
    nelson.unittest.report(results, 'Format', 'console', 'Verbose', run_resultVerbose(results));
  else
    varargout{1} = results;
  end
end
