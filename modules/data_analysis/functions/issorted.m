%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = issorted(varargin)
  narginchk(1, 3);
  A = varargin{1};
  if nargin == 1
    % TF = issorted(A)
    tf = all(diff(A) >= 0, 'all');
  elseif nargin == 2
    % TF = issorted(A, dim)
    % TF = issorted(A, 'rows')
    % TF = issorted(A, direction)
    numOrString = varargin{2};
    isrows = false;
    isdescend = false;
    isascend = false;
    if ischar(numOrString)
      isrows = strcmpi(numOrString, 'rows');
      isdescend = strcmpi(numOrString, 'descend');
      isascend = strcmpi(numOrString, 'ascend');
      isvalid = isrows || isdescend || isascend;
      if ~isvalid
        error(_('invalid argument: rows, descend, ascend expected.'));
      end
    else
      if isnumeric(numOrString) && isscalar(numOrString)
        dim = numOrString;
      else
        error(_('Invalid argument #2: scalar integer value expected.'));
      end
    end
    if (isrows || isdescend || isascend)
      if isrows
        tf = all(diff(A, 1, 1) >= 0, 'all');
      elseif isdescend
        tf = all(diff(A) <= 0, 'all');
      else
        tf = all(all(diff(A) >= 0, 2), 'all');
      end
    else
      tf = all(all(diff(A, [], dim) <= 0, dim), 'all');
    end
  else
    % TF = issorted(A, dim, direction)
    dim = varargin{2};
    if (~(isnumeric(dim) && isscalar(dim)))
      error(_('Invalid argument #2: scalar integer value expected.'));
    end
    direction = varargin{3};
    isdescend = strcmpi(direction, 'descend');
    isascend = strcmpi(direction, 'ascend');
    isvalid = isdescend || isascend;
    if ~isvalid
      error(_('invalid argument: descend, ascend expected.'));
    end
    if isdescend
      tf = all(diff(A, [], dim) <= 0, 'all');
    else
      tf = all(diff(A, [], dim) >= 0, 'all');
    end
  end
end
%=============================================================================
