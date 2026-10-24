%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [p, tbl, stats] = anova2(Y, reps, displayopt)
  narginchk(1, 3);
  nargoutchk(0, 3);
  hasDisplay = nargin >= 3;
  if nargin < 2
    reps = 1;
  elseif nargin == 2 && isDisplayOptionLocal(reps)
    displayopt = reps;
    reps = 1;
    hasDisplay = true;
  end
  if ~hasDisplay
    displayopt = 'on';
  end
  displayopt = parseDisplayLocal(displayopt);
  Y = validateDataLocal(Y);
  reps = validateRepsLocal(reps, size(Y, 1));
  result = anova2TableLocal(Y, reps);
  p = result.p;
  tbl = result.tbl;
  stats = result.stats;
  if strcmp(displayopt, 'on')
    disp(tbl);
  end
end
%=============================================================================
function Y = validateDataLocal(Y)
  if ~(isnumeric(Y) && isreal(Y) && ~issparse(Y) && ndims(Y) == 2)
    error('Nelson:statistics:yRealNumericMatrix', '%s must be a real numeric matrix.', 'Y');
  end
  Y = double(Y);
  if isempty(Y) || any(~isfinite(Y(:)))
    error('Nelson:statistics:yContainFiniteObservations', 'Y must contain finite observations.');
  end
  if size(Y, 1) < 2 || size(Y, 2) < 2
    error('Nelson:statistics:yLeastTwoRowsTwo', '%s must have at least two rows and two columns.', 'Y');
  end
end
%=============================================================================
function reps = validateRepsLocal(reps, nrows)
  if ~(isnumeric(reps) && isreal(reps) && isscalar(reps) && isfinite(reps) ...
      && reps >= 1 && floor(reps) == reps)
    error('Nelson:statistics:REPSPositiveIntegerScalar', '%s must be a positive integer scalar.', 'REPS');
  end
  reps = double(reps);
  if rem(nrows, reps) ~= 0
    error('Nelson:statistics:numberRowsYMultipleREPS', '%s must be a multiple of REPS.', 'The number of rows in Y');
  end
  if nrows / reps < 2
    error('Nelson:statistics:yContainLeastTwoRow', 'Y must contain at least two row-factor levels.');
  end
end
%=============================================================================
function result = anova2TableLocal(Y, reps)
  nRowLevels = size(Y, 1) / reps;
  nColLevels = size(Y, 2);
  grandMean = sum(Y(:)) / numel(Y);
  if reps == 1
    result = noReplicationLocal(Y, grandMean);
  else
    result = replicatedLocal(Y, reps, nRowLevels, nColLevels, grandMean);
  end
end
%=============================================================================
function result = noReplicationLocal(Y, grandMean)
  nRows = size(Y, 1);
  nCols = size(Y, 2);
  rowMeans = (Y * ones(nCols, 1)) / nCols;
  colMeans = (ones(1, nRows) * Y) / nRows;
  ssRows = nCols * sum((rowMeans - grandMean) .^ 2);
  ssCols = nRows * sum((colMeans - grandMean) .^ 2);
  ssTotal = sum((Y(:) - grandMean) .^ 2);
  ssError = ssTotal - ssRows - ssCols;
  [dfRows, dfCols, dfError] = noReplicationDfLocal(nRows, nCols);
  [msRows, msCols, msError, fRows, fCols, pRows, pCols] = ...
    ratiosLocal(ssRows, ssCols, ssError, dfRows, dfCols, dfError);
  result.p = [pCols, pRows];
  result.tbl = {'Source', 'SS', 'df', 'MS', 'F', 'Prob>F'; ...
                'Columns', ssCols, dfCols, msCols, fCols, pCols; ...
                'Rows', ssRows, dfRows, msRows, fRows, pRows; ...
                'Error', ssError, dfError, msError, [], []; ...
                'Total', ssTotal, nRows * nCols - 1, [], [], []};
  result.stats = statsLocal(rowMeans(:)', colMeans(:), [], msError, dfError);
end
%=============================================================================
function [dfRows, dfCols, dfError] = noReplicationDfLocal(nRows, nCols)
  dfRows = nRows - 1;
  dfCols = nCols - 1;
  dfError = dfRows * dfCols;
end
%=============================================================================
function result = replicatedLocal(Y, reps, nRowLevels, nColLevels, grandMean)
  [cellMeans, rowMeans, colMeans, ssError] = replicatedMeansLocal(Y, reps, nRowLevels, nColLevels);
  ssRows = nColLevels * reps * sum((rowMeans - grandMean) .^ 2);
  ssCols = nRowLevels * reps * sum((colMeans - grandMean) .^ 2);
  ssInteraction = reps * interactionSumLocal(cellMeans, rowMeans, colMeans, grandMean);
  ssTotal = sum((Y(:) - grandMean) .^ 2);
  [dfRows, dfCols, dfInteraction, dfError] = replicatedDfLocal(nRowLevels, nColLevels, reps);
  msRows = ssRows / dfRows;
  msCols = ssCols / dfCols;
  msInteraction = ssInteraction / dfInteraction;
  msError = ssError / dfError;
  fRows = msRows / msError;
  fCols = msCols / msError;
  fInteraction = msInteraction / msError;
  pRows = fcdf(fRows, dfRows, dfError, 'upper');
  pCols = fcdf(fCols, dfCols, dfError, 'upper');
  pInteraction = fcdf(fInteraction, dfInteraction, dfError, 'upper');
  result.p = [pCols, pRows, pInteraction];
  result.tbl = replicatedTableLocal(ssCols, ssRows, ssInteraction, ssError, ssTotal, ...
    dfCols, dfRows, dfInteraction, dfError, msCols, msRows, msInteraction, msError, ...
    fCols, fRows, fInteraction, pCols, pRows, pInteraction);
  result.stats = statsLocal(rowMeans(:)', colMeans(:), cellMeans, msError, dfError);
end
%=============================================================================
function [cellMeans, rowMeans, colMeans, ssError] = replicatedMeansLocal(Y, reps, nRows, nCols)
  cellMeans = zeros(nRows, nCols);
  rowSums = zeros(nRows, 1);
  colSums = zeros(1, nCols);
  ssError = 0;
  for i = 1:nRows
    rows = (i - 1) * reps + 1:i * reps;
    block = Y(rows, :);
    cellMeans(i, :) = (ones(1, reps) * block) / reps;
    rowSums(i) = sum(block(:));
    colSums = colSums + ones(1, reps) * block;
    centered = block - repmat(cellMeans(i, :), reps, 1);
    ssError = ssError + sum(centered(:) .^ 2);
  end
  rowMeans = rowSums / (nCols * reps);
  colMeans = colSums / (nRows * reps);
end
%=============================================================================
function value = interactionSumLocal(cellMeans, rowMeans, colMeans, grandMean)
  value = 0;
  for i = 1:size(cellMeans, 1)
    delta = cellMeans(i, :) - rowMeans(i) - colMeans + grandMean;
    value = value + sum(delta .^ 2);
  end
end
%=============================================================================
function [dfRows, dfCols, dfInteraction, dfError] = replicatedDfLocal(nRows, nCols, reps)
  dfRows = nRows - 1;
  dfCols = nCols - 1;
  dfInteraction = dfRows * dfCols;
  dfError = nRows * nCols * (reps - 1);
end
%=============================================================================
function [msRows, msCols, msError, fRows, fCols, pRows, pCols] = ...
    ratiosLocal(ssRows, ssCols, ssError, dfRows, dfCols, dfError)
  msRows = ssRows / dfRows;
  msCols = ssCols / dfCols;
  msError = ssError / dfError;
  fRows = msRows / msError;
  fCols = msCols / msError;
  pRows = fcdf(fRows, dfRows, dfError, 'upper');
  pCols = fcdf(fCols, dfCols, dfError, 'upper');
end
%=============================================================================
function tbl = replicatedTableLocal(ssCols, ssRows, ssInter, ssError, ssTotal, dfCols, ...
    dfRows, dfInter, dfError, msCols, msRows, msInter, msError, fCols, fRows, fInter, ...
    pCols, pRows, pInter)
  tbl = {'Source', 'SS', 'df', 'MS', 'F', 'Prob>F'; ...
         'Columns', ssCols, dfCols, msCols, fCols, pCols; ...
         'Rows', ssRows, dfRows, msRows, fRows, pRows; ...
         'Interaction', ssInter, dfInter, msInter, fInter, pInter; ...
         'Error', ssError, dfError, msError, [], []; ...
         'Total', ssTotal, dfCols + dfRows + dfInter + dfError, [], [], []};
end
%=============================================================================
function stats = statsLocal(rowMeans, colMeans, cellMeans, mse, dfe)
  stats = struct('source', 'anova2', 'sigmasq', mse, 'df', dfe, ...
    'rowmeans', rowMeans, 'colmeans', colMeans, 'cellmeans', cellMeans);
end
%=============================================================================
function opt = parseDisplayLocal(value)
  if ~isDisplayOptionLocal(value)
    error('Nelson:statistics:displayOptionOff', 'Display option must be ''on'' or ''off''.');
  end
  opt = lower(char(value));
end
%=============================================================================
function tf = isDisplayOptionLocal(value)
  tf = false;
  if isTextScalarLocal(value)
    text = lower(char(value));
    tf = strcmp(text, 'on') || strcmp(text, 'off');
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
