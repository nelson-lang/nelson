%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = tuneWeights(targetsOrResults, varargin)
  if nargout > 2
    error('Nelson:tooManyOutputs', 'Too many output arguments.');
  end
  [options, runArguments] = tuneWeights_parseOptions(varargin); %#ok<NLS0004>
  if tuneWeights_isResult(targetsOrResults) %#ok<NLS0004>
    if ~isempty(runArguments)
      error('Nelson:tests_manager:invalidArgument:executionOptionsAreNotAcceptedA', 'Execution options are not accepted with a TestRunResult.');
    end
    results = targetsOrResults;
  else
    results = nelson.unittest.run(targetsOrResults, runArguments{:});
  end
  report = tuneWeights_buildReport(results, options); %#ok<NLS0004>
  if nargout == 0
    tuneWeights_display(report); %#ok<NLS0004>
    return
  end
  varargout{1} = report;
  if nargout > 1
    varargout{2} = results;
  end
end
%=============================================================================
