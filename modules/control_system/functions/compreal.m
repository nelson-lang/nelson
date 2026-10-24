%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = compreal(varargin)
  narginchk(2, 2);
  nargoutchk(0, 5);
  numerator = varargin{1};
  denominator = varargin{2};
  numeratorShape = size(numerator);
  outputCount = numeratorShape(1);
  coefficientCount = numeratorShape(2);

  if coefficientCount == 1 || norm(numerator, 1) == 0
    realization = pureGainRealization(numerator, denominator, outputCount);
  else
    realization = companionRealization(numerator, denominator, coefficientCount, outputCount);
  end
  realization = scaledRealization(realization);

  varargout{1} = realization.A;
  if nargout > 1
    varargout{2} = realization.B;
  end
  if nargout > 2
    varargout{3} = realization.C;
  end
  if nargout > 3
    varargout{4} = realization.D;
  end
  if nargout > 4
    varargout{5} = realization.E;
  end
end
%=============================================================================
function realization = pureGainRealization(numerator, denominator, outputCount)
  realization.A = [];
  realization.B = zeros(0, 1);
  realization.C = zeros(outputCount, 0);
  realization.D = numerator(:, 1) / denominator(:, 1);
  realization.E = [];
  realization.kE = 0;
end
%=============================================================================
function realization = companionRealization(numerator, denominator, coefficientCount, outputCount)
  leadingIndex = find(denominator ~= 0, 1);
  leadingCoefficient = denominator(leadingIndex);
  scaledNumerator = numerator / leadingCoefficient;
  scaledDenominator = denominator(leadingIndex + 1:coefficientCount) / leadingCoefficient;

  if leadingIndex == 1
    realization = properCompanion(scaledNumerator, scaledDenominator, coefficientCount);
  else
    realization = improperCompanion(scaledNumerator, scaledDenominator, leadingIndex, outputCount);
  end
end
%=============================================================================
function realization = properCompanion(numerator, denominator, coefficientCount)
  denominatorLength = length(denominator);
  realization.A = [-denominator; eye(denominatorLength - 1, denominatorLength)];
  realization.B = eye(denominatorLength, 1);
  realization.D = numerator(:, 1);
  realization.C = numerator(:, 2:coefficientCount) - realization.D * denominator;
  realization.E = [];
  realization.kE = 0;
end
%=============================================================================
function realization = improperCompanion(numerator, denominator, leadingIndex, outputCount)
  denominatorLength = length(denominator);
  shiftRows = min(1, denominatorLength);
  realization.A = blkdiag(eye(leadingIndex), diag(ones(shiftRows, denominatorLength - 1), -1));
  realization.A(leadingIndex + shiftRows, leadingIndex + 1:leadingIndex + denominatorLength) ...
    = -denominator;
  realization.B = zeros(leadingIndex + denominatorLength, 1);
  if denominatorLength <= 0
    realization.B(leadingIndex) = -1;
  else
    realization.B(leadingIndex + 1) = 1;
  end
  realization.C = numerator;
  realization.D = zeros(outputCount, 1);
  realization.E = diag([zeros(leadingIndex, 1); ones(denominatorLength, 1)]);
  stride = leadingIndex + denominatorLength + 1;
  realization.E(stride:stride:(leadingIndex - 1 + shiftRows) * stride) = 1;
  realization.kE = leadingIndex;
end
%=============================================================================
function realization = scaledRealization(realization)
  stateCount = size(realization.A, 1);
  descriptorCount = realization.kE;

  scaleRatios = stateScaleRatios(realization.A, descriptorCount, stateCount);
  if descriptorCount > 0
    descriptorScales = descriptorScaleRatios(realization.C, descriptorCount, stateCount);
    descriptorCount = descriptorScales.descriptorCount;
    scaleRatios = [descriptorScales.ratios; scaleRatios];
  end

  realization = applyStateScaling(realization, scaleRatios);
  realization = normalizeInputOutputGain(realization);
end
%=============================================================================
function realization = applyStateScaling(realization, ratios)
  stateScales = cumprod([1; ratios]);
  inverseScales = 1 ./ stateScales;
  realization.A = scaleMatrix(realization.A, stateScales, inverseScales);
  realization.B = scaleMatrix(realization.B, stateScales, []);
  realization.C = scaleMatrix(realization.C, [], inverseScales);
  if ~isempty(realization.E)
    realization.E = scaleMatrix(realization.E, stateScales, inverseScales);
  end
end
%=============================================================================
function realization = normalizeInputOutputGain(realization)
  outputMagnitude = norm(realization.C, 1);
  inputMagnitude = norm(realization.B, 1);
  if outputMagnitude > 0
    gainScale = pow2(round(log2(outputMagnitude / inputMagnitude) / 2));
    realization.C = realization.C / gainScale;
    realization.B = realization.B * gainScale;
  end
end
%=============================================================================
function ratios = stateScaleRatios(A, descriptorCount, stateCount)
  if stateCount - descriptorCount < 2
    ratios = zeros(0, 1);
  else
    %#ok<NLS0004>
    [~, ~, balanceMatrix] = balance(A(descriptorCount + 1:stateCount, ...
      descriptorCount + 1:stateCount), 'noperm');
    ratios = diag(balanceMatrix, -1);
  end

  smallDrop = find(ratios(2:end) < min(1, 1e-3 * ratios(1:end - 1)));
  if ~isempty(smallDrop)
    floorValue = min(1, ratios(smallDrop(1)));
    ratios = max(ratios, floorValue);
  end
end
%=============================================================================
function result = descriptorScaleRatios(C, initialDescriptorCount, stateCount)
  descriptorCount = initialDescriptorCount;
  if descriptorCount < stateCount
    descriptorCount = descriptorCount + 1;
  end
  columnNorms = max(abs(C(:, 1:descriptorCount)), [], 1);
  columnNorms = columnNorms / columnNorms(1);
  %#ok<NLS0004>
  [~, ~, balanceMatrix] = balance([columnNorms; eye(descriptorCount - 1, descriptorCount)], ...
    'noperm');
  result.ratios = diag(balanceMatrix, -1);
  result.descriptorCount = descriptorCount;
end
%=============================================================================
function scaled = scaleMatrix(X, requestedRowScales, requestedColumnScales)
  rowScales = completeRowScales(X, requestedRowScales);
  columnScales = completeColumnScales(X, requestedColumnScales);
  scaled = X .* (rowScales * columnScales);
end
%=============================================================================
function rowScales = completeRowScales(X, rowScales)
  if size(rowScales, 2) == 0
    rowCount = size(X, 1);
    if rowCount > 0
      rowScales = ones(rowCount, 1);
    else
      rowScales = [];
    end
  elseif size(rowScales, 2) > 1
    rowScales = rowScales.';
  end
end
%=============================================================================
function columnScales = completeColumnScales(X, columnScales)
  if size(columnScales, 1) == 0
    columnCount = size(X, 2);
    if columnCount > 0
      columnScales = ones(1, columnCount);
    else
      columnScales = [];
    end
  elseif size(columnScales, 1) > 1
    columnScales = columnScales.';
  end
end
%=============================================================================
