%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function optimizer = ip_registration_optimizer(optimizer)
  if ~isstruct(optimizer)
    error('Nelson:image_processing:registrationOptimizerStructure', 'Registration optimizer must be a structure.');
  end
  if ~isfield(optimizer, 'MaximumIterations')
    optimizer.MaximumIterations = 50;
  end
  if ~isnumeric(optimizer.MaximumIterations) || ~isreal(optimizer.MaximumIterations) ...
      || numel(optimizer.MaximumIterations) ~= 1 || optimizer.MaximumIterations < 1 ...
      || fix(optimizer.MaximumIterations) ~= optimizer.MaximumIterations
    error('Nelson:image_processing:maximumiterationsPositiveIntegerScalar', '%s must be a positive integer scalar.', 'MaximumIterations');
  end
  if ~isfield(optimizer, 'AngleSearch')
    optimizer.AngleSearch = 15;
  end
  if ~isfield(optimizer, 'ScaleSearch')
    optimizer.ScaleSearch = 0.05;
  end
  if ~isfield(optimizer, 'ShearSearch')
    optimizer.ShearSearch = 0.05;
  end
  if ~isnumeric(optimizer.AngleSearch) || ~isreal(optimizer.AngleSearch) ...
      || numel(optimizer.AngleSearch) ~= 1 || ~isfinite(optimizer.AngleSearch) ...
      || optimizer.AngleSearch < 0
    error('Nelson:image_processing:anglesearchNonnegativeFiniteScalar', '%s must be a nonnegative finite scalar.', 'AngleSearch');
  end
  if ~isnumeric(optimizer.ScaleSearch) || ~isreal(optimizer.ScaleSearch) ...
      || numel(optimizer.ScaleSearch) ~= 1 || ~isfinite(optimizer.ScaleSearch) ...
      || optimizer.ScaleSearch < 0 || optimizer.ScaleSearch >= 1
    error('Nelson:image_processing:scalesearchFiniteScalarRange', '%s must be a finite scalar in the range [%s].', 'ScaleSearch', '0, 1');
  end
  if ~isnumeric(optimizer.ShearSearch) || ~isreal(optimizer.ShearSearch) ...
      || numel(optimizer.ShearSearch) ~= 1 || ~isfinite(optimizer.ShearSearch) ...
      || optimizer.ShearSearch < 0 || optimizer.ShearSearch >= 1
    error('Nelson:image_processing:shearsearchFiniteScalarRange', '%s must be a finite scalar in the range [%s].', 'ShearSearch', '0, 1');
  end
end
%=============================================================================
