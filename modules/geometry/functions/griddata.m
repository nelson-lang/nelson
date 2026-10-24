%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [out1, out2, out3] = griddata(varargin)
  nargoutchk(0, 3);
  narginchk(4, 8);
  method = 'linear';
  if ischar(varargin{end}) || (isa(varargin{end}, 'string') && isscalar(varargin{end}))
    method = char(varargin{end});
    varargin = varargin(1:end - 1);
  end
  if length(varargin) == 4
    points = varargin{1};
    values = varargin{2}(:);
    querySize = size(varargin{3});
    query = [varargin{3}(:), varargin{4}(:)];
    xq = varargin{3};
    yq = varargin{4};
  elseif length(varargin) == 5
    points = [varargin{1}(:), varargin{2}(:)];
    values = varargin{3}(:);
    querySize = size(varargin{4});
    query = [varargin{4}(:), varargin{5}(:)];
    xq = varargin{4};
    yq = varargin{5};
  elseif length(varargin) == 7
    points = [varargin{1}(:), varargin{2}(:), varargin{3}(:)];
    values = varargin{4}(:);
    querySize = size(varargin{5});
    query = [varargin{5}(:), varargin{6}(:), varargin{7}(:)];
    xq = varargin{5};
    yq = varargin{6};
  else
    error('Nelson:geometry:wrongNumberInputArguments', 'Wrong number of input arguments.');
  end
  interpolant = scatteredInterpolant(points, values, method, 'none');
  vq = reshape(evaluate(interpolant, query), querySize);
  if nargout <= 1
    out1 = vq;
  else
    out1 = xq;
    out2 = yq;
    out3 = vq;
  end
end
%=============================================================================
