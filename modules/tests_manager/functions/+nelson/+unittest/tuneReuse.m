%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = tuneReuse(targets, varargin)
  if nargout > 2
    error('Nelson:tooManyOutputs', 'Too many output arguments.');
  end
  [options, runArguments] = tuneReuse_parseOptions(varargin); %#ok<NLS0004>
  calibration = tuneReuse_calibrate(targets, runArguments, options); %#ok<NLS0004>
  report = tuneReuse_buildReport(calibration, options); %#ok<NLS0004>
  if nargout == 0
    tuneReuse_display(report); %#ok<NLS0004>
    return
  end
  varargout{1} = report;
  if nargout > 1
    varargout{2} = calibration;
  end
end
%=============================================================================
