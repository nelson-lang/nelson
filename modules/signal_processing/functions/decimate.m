%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = decimate(varargin)
  narginchk(2, 4);
  nargoutchk(0, 1);
  x = varargin{1};
  q = varargin{2};

  filterType = 'iir';
  if nargin >= 3 && (ischar(varargin{3}) || isstring(varargin{3}))
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Filter order');
  end

  if nargin >= 3
    n = varargin{3};
  else
    n = [];
  end

  if nargin == 4
    filterType = char(varargin{4});
  end

  mustBeNumeric(x);
  if isempty(x) || ~isvector(x)
    error('Nelson:signal:invalidInputSignal', 'Input signal must be a nonempty vector.');
  end

  mustBeNumeric(q);
  mustBeScalarOrEmpty(q);
  mustBeFinite(q);
  mustBeInteger(q);
  mustBePositive(q);
  if q < 2
    error('Nelson:signal:decimate:invalidFactor', 'Decimation factor must be greater than one.');
  end

  if isempty(n)
    if strcmpi(filterType, 'fir')
      n = 30;
    else
      n = 8;
    end
  end
  mustBeNumeric(n);
  mustBeScalarOrEmpty(n);
  mustBeFinite(n);
  mustBeInteger(n);
  mustBePositive(n);

  if strcmpi(filterType, 'fir')
    y = signalDecimateFir(x, q, n);
  elseif strcmpi(filterType, 'iir')
    y = signalDecimateIir(x, q, n);
  else
    error('Nelson:signal:decimate:invalidFilterType', 'Filter type must be ''iir'' or ''fir''.');
  end
end
%=============================================================================
