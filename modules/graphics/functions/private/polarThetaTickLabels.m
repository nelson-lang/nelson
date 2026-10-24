%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function labels = polarThetaTickLabels(ticks, thetaLim, thetaAxisUnits)
  if nargin < 3
    thetaAxisUnits = 'degrees';
  end
  labelTicks = ticks;
  if isFullCircle(thetaLim, thetaAxisUnits) && numel(labelTicks) > 1 ...
      && abs(labelTicks(end) - thetaLim(2)) < 1e-12
    labelTicks(end) = [];
  end
  if strcmp(thetaAxisUnits, 'radians')
    labels = polarFormatPiLabels(labelTicks);
  else
    labels = polarFormatTickLabels(labelTicks, true);
  end
end
%=============================================================================
function labels = polarFormatPiLabels(values)
  labels = cell(numel(values), 1);
  for k = 1:numel(values)
    labels{k} = polarPiLabel(values(k));
  end
end
%=============================================================================
function label = polarPiLabel(value)
  ratio = value / pi;
  for denominator = 1:12
    numerator = round(ratio * denominator);
    if abs(ratio - numerator / denominator) < 1e-10
      label = polarPiFractionLabel(numerator, denominator);
      return
    end
  end
  label = polarFormatTickLabels(value, false);
  label = label{1};
end
%=============================================================================
function label = polarPiFractionLabel(numerator, denominator)
  if numerator == 0
    label = '0';
    return
  end
  divisor = gcdLocal(abs(numerator), denominator);
  numerator = numerator / divisor;
  denominator = denominator / divisor;
  prefix = '';
  if numerator < 0
    prefix = '-';
  end
  numerator = abs(numerator);
  coefficient = '';
  if numerator ~= 1
    coefficient = int2str(numerator);
  end
  if denominator == 1
    label = [prefix, coefficient, '\pi'];
  else
    label = [prefix, coefficient, '\pi/', int2str(denominator)];
  end
end
%=============================================================================
function value = gcdLocal(a, b)
  while b ~= 0
    t = rem(a, b);
    a = b;
    b = t;
  end
  if a == 0
    value = 1;
  else
    value = a;
  end
end
%=============================================================================
function tf = isFullCircle(thetaLim, thetaAxisUnits)
  fullCircle = 360;
  if strcmp(thetaAxisUnits, 'radians')
    fullCircle = 2 * pi;
  end
  tf = abs(abs(thetaLim(2) - thetaLim(1)) - fullCircle) < 1e-12;
end
%=============================================================================
