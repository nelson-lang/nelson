%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function display(varargin)
  sys = varargin{1};
  if nargin == 2
    name = varargin{2};
  else
    name = inputname(1);
  end
  currentFormat = format();
  if ~isempty(name)
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
    disp([name, ' ='])
  end
  if strcmp(currentFormat.LineSpacing, 'loose')
    disp(' ');
  end
  Z = sys.Z;
  P = sys.P;
  K = sys.K;
  if isempty(K)
    disp(['  ', _('Empty zero-pole-gain model.')])
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
    return
  end
  [m, p] = size(Z);
  variableName = sys.Variable;
  for q = 1:p
    if (p ~= 1)
      disp(['  ', sprintf(_('From input %d to output:'), q)]);
    end
    for k = 1:m
      gain = K(min(k, size(K, 1)), min(q, size(K, 2)));
      numeratorString = zpkCombineGainFactors(gain, zpkFactorString(Z{k, q}, variableName));
      denominatorString = zpkFactorString(P{k, q}, variableName);
      if isempty(denominatorString)
        denominatorString = '1';
      end
      displayFraction(numeratorString, denominatorString, m, k);
      if strcmp(currentFormat.LineSpacing, 'loose')
        disp(' ');
      end
    end
  end
  displayTrailer(sys, currentFormat);
end
%=============================================================================
function displayFraction(numeratorString, denominatorString, m, k)
  isOneString = strcmp(denominatorString, '1');
  maxLen = max([length(denominatorString), length(numeratorString)]);
  numeratorString = [repmat(' ', 1, maxLen - length(numeratorString)), numeratorString];
  denominatorString = [repmat(' ', 1, maxLen - length(denominatorString)), denominatorString];
  ce = strjust({numeratorString, denominatorString}, 'center');
  numeratorString = deblank(ce{1});
  denominatorString = deblank(ce{2});
  dashString = getDashedLine(numeratorString, denominatorString);
  if (m ~= 1)
    disp(['  ', sprintf('%d:', k)])
  end
  if isOneString
    disp(['  ', numeratorString]);
  else
    disp(['  ', numeratorString]);
    disp(['  ', dashString]);
    disp(['  ', denominatorString]);
  end
end
%=============================================================================
function displayTrailer(sys, currentFormat)
  if isstatic(sys)
    disp(['     ', _('Static gain.')]);
    displayModelProperties(sys, [_('Model Properties'), char(10)]);
  else
    if isdt(sys)
      disp(sprintf(_('Sample time: %.4f %s'), sys.Ts, sys.TimeUnit));
      disp(_('Discrete-time zero-pole-gain model.'));
    else
      disp(_('Continuous-time zero-pole-gain model.'));
    end
    displayModelProperties(sys, '');
  end
  if strcmp(currentFormat.LineSpacing, 'loose')
    disp(' ');
  end
end
%=============================================================================
function str = zpkCombineGainFactors(gain, factors)
  if isempty(factors)
    str = num2str(gain);
    return
  end
  if gain == 1
    str = factors;
  elseif gain == -1
    str = ['-', factors];
  else
    str = [num2str(gain), ' ', factors];
  end
end
%=============================================================================
function str = zpkFactorString(roots, variableName)
  str = '';
  roots = roots(:);
  if isempty(roots)
    return
  end
  tol = 1e-10 * max([1; abs(roots)]);
  factors = {};
  used = false(numel(roots), 1);
  for i = 1:numel(roots)
    if used(i)
      continue
    end
    r = roots(i);
    used(i) = true;
    if abs(imag(r)) <= tol
      factors{end + 1} = oneRootFactor(real(r), variableName);
    else
      partner = findConjugate(roots, used, r, tol);
      if partner > 0
        used(partner) = true;
        factors{end + 1} = quadFactor(real(r), abs(imag(r)), variableName);
      else
        factors{end + 1} = complexLinearFactor(r, variableName);
      end
    end
  end
  str = groupAndJoinFactors(factors);
end
%=============================================================================
function partner = findConjugate(roots, used, r, tol)
  partner = 0;
  for j = 1:numel(roots)
    if used(j)
      continue
    end
    if abs(roots(j) - conj(r)) <= tol
      partner = j;
      return
    end
  end
end
%=============================================================================
function str = groupAndJoinFactors(factors)
  uniqueFactors = {};
  counts = [];
  for i = 1:numel(factors)
    idx = find(strcmp(uniqueFactors, factors{i}), 1);
    if isempty(idx)
      uniqueFactors{end + 1} = factors{i};
      counts(end + 1) = 1;
    else
      counts(idx) = counts(idx) + 1;
    end
  end
  str = '';
  for i = 1:numel(uniqueFactors)
    piece = uniqueFactors{i};
    if counts(i) > 1
      piece = [piece, '^', num2str(counts(i))];
    end
    if isempty(str)
      str = piece;
    else
      str = [str, ' ', piece];
    end
  end
end
%=============================================================================
function base = oneRootFactor(rootValue, variableName)
  if rootValue < 0
    base = ['(', variableName, ' + ', num2str(-rootValue), ')'];
  elseif rootValue > 0
    base = ['(', variableName, ' - ', num2str(rootValue), ')'];
  else
    base = variableName;
  end
end
%=============================================================================
function base = quadFactor(a, b, variableName)
  c1 = -2 * a;
  c0 = a * a + b * b;
  base = ['(', variableName, '^2'];
  if c1 ~= 0
    magnitude = abs(c1);
    if magnitude == 1
      term = variableName;
    else
      term = [num2str(magnitude), variableName];
    end
    if c1 > 0
      base = [base, ' + ', term];
    else
      base = [base, ' - ', term];
    end
  end
  if c0 ~= 0
    if c0 > 0
      base = [base, ' + ', num2str(c0)];
    else
      base = [base, ' - ', num2str(-c0)];
    end
  end
  base = [base, ')'];
end
%=============================================================================
function base = complexLinearFactor(rootValue, variableName)
  base = ['(', variableName, ' - (', num2str(real(rootValue)), ...
    signChar(imag(rootValue)), num2str(abs(imag(rootValue))), 'i))'];
end
%=============================================================================
function c = signChar(value)
  if value < 0
    c = '-';
  else
    c = '+';
  end
end
%=============================================================================
function dash = getDashedLine(numeratorString, denominatorString)
  dash = '';
  lenDash = max(length(numeratorString), length(denominatorString));
  if ~mod(lenDash, 2)
    lenDash = lenDash + 1;
  end
  for i = 1:lenDash
    dash = strcat(dash, '—');
  end
end
%=============================================================================
