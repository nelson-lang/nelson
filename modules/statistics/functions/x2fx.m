%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function D = x2fx(X, modelspec, varargin)
  narginchk(2, 4);
  nargoutchk(0, 1);

  if ~isnumeric(X) || ndims(X) ~= 2
    error('Nelson:statistics:xNumericMatrix', '%s must be a numeric matrix.', 'X');
  end
  terms = modelTerms(size(X, 2), modelspec);
  D = ones(size(X, 1), 1);
  for t = 1:size(terms, 1)
    col = ones(size(X, 1), 1);
    for j = 1:size(X, 2)
      if terms(t, j) ~= 0
        col = col .* (X(:, j) .^ terms(t, j));
      end
    end
    D = [D col];
  end
end
%=============================================================================
function terms = modelTerms(p, modelspec)
  if ischar(modelspec) || isStringScalar(modelspec)
    text = lower(char(modelspec));
    switch text
      case {'constant', 'const'}
        terms = zeros(0, p);
      case 'linear'
        terms = eye(p);
      case {'interaction', 'interactions'}
        terms = [eye(p); pairTerms(p)];
      case {'quadratic', 'purequadratic'}
        terms = [eye(p); 2 * eye(p)];
      otherwise
        error('Nelson:statistics:invalidModelSpecification', 'Invalid model specification.');
    end
  elseif isnumeric(modelspec)
    if isscalar(modelspec)
      degree = positiveInteger(modelspec, sprintf(_('%s must be a positive integer scalar.'), 'Model degree'));
      terms = degreeTerms(p, degree);
    else
      if size(modelspec, 2) ~= p
        error('Nelson:statistics:modelMatrixOneColumnFactor', 'Model matrix must have one column per %s.', 'factor');
      end
      terms = modelspec;
    end
  else
    error('Nelson:statistics:invalidModelSpecification', 'Invalid model specification.');
  end
end
%=============================================================================
function terms = degreeTerms(p, degree)
  terms = [];
  for total = 1:degree
    terms = [terms; compositions(p, total)];
  end
end
%=============================================================================
function terms = compositions(p, total)
  if p == 1
    terms = total;
    return
  end
  terms = [];
  for v = 0:total
    rest = compositions(p - 1, total - v);
    terms = [terms; [v * ones(size(rest, 1), 1) rest]];
  end
end
%=============================================================================
function terms = pairTerms(p)
  terms = [];
  for i = 1:p
    for j = i + 1:p
      row = zeros(1, p);
      row(i) = 1;
      row(j) = 1;
      terms = [terms; row];
    end
  end
end
%=============================================================================
function value = positiveInteger(value, message)
  if ~isscalar(value) || value <= 0 || floor(value) ~= value
    error('Nelson:statistics:mustBePositiveInteger', message);
  end
end
%=============================================================================
