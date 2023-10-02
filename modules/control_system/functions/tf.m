%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: MIT OR LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = tf(varargin)
  narginchk(0, 3);
  nargoutchk(0, 1);
  sys = [];
  if (nargin == 0)
    sys = tf_no_rhs();
  end
  if (nargin == 1)
    sys = tf_one_rhs(varargin{1});
  end
  if (nargin == 2)
    sys = tf_two_rhs(varargin{1}, varargin{2});
  end
  if (nargin == 3)
    sys = tf_three_rhs(varargin{1}, varargin{2}, varargin{3});
  end
  if (~isa(sys, 'tf'))
    error(_('Wrong number of input arguments.'));
  end
  varargout{1} = sys;
end
%=============================================================================
function sys = tf_no_rhs()
  tf = {};
  tf.Numerator = {};
  tf.Denominator= {};
  tf.Variable = 's';
  tf.Ts = 0;
  tf.TimeUnit = 'seconds';
  tf.UserData = [];
  tf.Internal = [];
  tf.Internal.Version = 1;
  sys = class(tf, 'tf');
end
%=============================================================================
function sys = tf_one_rhs(m)
  if ~isnumeric(m)
    error('numeric value expected.');
  end
  sys = tf_no_rhs();
  sys.Denominator = num2cell(ones(size(m)));
  sys.Numerator = num2cell(m);
  sys.Ts = -2;
end
%=============================================================================
function sys = tf_two_rhs(numerator, denominator)
  numerator = checkFractionPart(numerator);
  denominator = checkFractionPart(denominator);
  if (length(numerator) == 1)
    for k = 2:length(denominator)
      numerator{k} = numerator{1};
    end
  end
  if (length(denominator) == 1)
    for k = 2:length(numerator)
      denominator{k} = denominator{1};
    end
  end
  if (length(numerator) ~= length(denominator))
    error(_('Numerator and Denominator must have compatible sizes.'));
  end
  for k = 1:length(numerator)
    [N, D] = arrangeVectors(numerator{k}, denominator{k});
    numerator{k} = N;
    denominator{k} = D;
  end
  sys = tf_no_rhs();
  sys.Denominator = denominator;
  sys.Numerator = numerator;
end
%=============================================================================
function sys = tf_three_rhs(numerator, denominator, Ts)
  sys = tf_two_rhs(numerator, denominator);
  if (Ts > 0)
    sys.Variable = 'z';
    sys.Ts = Ts;
  end
end
%=============================================================================
function modifiedPart = checkFractionPart(part)
  if iscell(part)
    if isvector(part)
      part = part(:)';
    else
      error(_('Invalid syntax.'));
    end
  elseif isnumeric(part)
    if isvector(part) || isempty(part)
      part = part(:)';
      part = {part};
    else
      error(_('Invalid syntax.'));
    end
  else
    error(_('Invalid syntax.'));
  end
  for j = 1:length(part)
    for i = 1:length(part{j})
      if(or(part{j}(i) > 0, part{j}(i) < 0))
        part{j} = part{j}(1, i:length(part{j}));
        break;
      end
    end
  end
  modifiedPart = part;
end
%=============================================================================
function [v1, v2] = arrangeVectors(v1, v2)
  v1 = v1(:)';
  v2 = v2(:)';
  lenV1 = length(v1);
  lenV2 = length(v2);
  if lenV2 ~= lenV1
    if (lenV2 > lenV1)
      lenDiff = lenV2 - lenV1;
      v1 = [zeros(1, lenDiff) , v1];
    end
    if (lenV2 < lenV1)
      lenDiff = lenV1 - lenV2;
      v2 = [zeros(1, lenDiff) , v2];
    end
  end
end
%=============================================================================
